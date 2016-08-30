module Routing.Bob.UrlBoomerang where

import Prelude
import Text.Boomerang.String as Text.Boomerang.String
import Text.Parsing.StringParser as StringParser
import Control.Error.Util (hush)
import Data.Either (Either(Right, Left))
import Data.Identity (Identity)
import Data.Maybe (Maybe(Nothing, Just))
import Data.StrMap (empty, insert, pop, StrMap)
import Data.Tuple (snd, Tuple(Tuple), fst)
import Data.URI (Query(Query), RelativePart(RelativePart), RelativeRef(RelativeRef), printRelativeRef, runParseRelativeRef)
import Data.URI.RelativePart (parseRelativePart, printRelativePart)
import Partial.Unsafe (unsafePartial)
import Text.Boomerang.Combinators (maph)
import Text.Boomerang.HStack (type (:-), (:-))
import Text.Boomerang.Prim (runSerializer, Serializer(Serializer), Boomerang(Boomerang))
import Text.Boomerang.String (many1NoneOf, string, StringBoomerang)
import Text.Parsing.Parser (parseFailed, runParser, ParserT(ParserT), unParserT, PState(PState), Parser)

-- we want to parse/serialize query and path at the same time
type Url =
  { path :: String
  , query :: StrMap (Maybe String)
  }

type UrlBoomerang a b = Boomerang Url a b

parseURL :: String -> Maybe Url
parseURL s =
  case runParseRelativeRef s of
    Right (RelativeRef (RelativePart _ maybePath) maybeQuery _) ->
      let
        query =
          case maybeQuery of
            Nothing -> empty
            (Just (Query q)) -> q
        path =
          case maybePath of
            Nothing -> ""
            Just p -> printRelativePart (RelativePart Nothing maybePath)
      in
        Just
          { query: query
          , path: path
          }
    (Left _) -> Nothing

printURL :: Url -> Maybe String
printURL { path, query } = do
  relativePart <- hush $ StringParser.runParser parseRelativePart path
  pure $ printRelativeRef (RelativeRef relativePart (Just (Query query)) Nothing)

liftStringPrs :: forall p a. Parser String a -> Parser { path :: String | p } a
liftStringPrs prs =
  ParserT prs'
 where
  prs' (PState i) = do
    result <- unParserT prs (PState { input: i.input.path, position: i.position })
    pure $ (result { input = i.input { path = result.input }})

liftStringSer :: forall p s s'. Serializer String s s' -> Serializer { path :: String | p } s s'
liftStringSer (Serializer ser) =
  Serializer ser'
 where
  ser' a = do
    t <- ser a
    let
      f = fst t
      f' r = r { path = f r.path }
    pure (Tuple f' (snd t))

liftStringBoomerang :: forall r r'. StringBoomerang r r' -> UrlBoomerang r r'
liftStringBoomerang (Boomerang ps) =
  Boomerang
    { prs: liftStringPrs ps.prs
    , ser: liftStringSer ps.ser
    }

param :: forall a r. String -> UrlBoomerang r (a :- r) -> UrlBoomerang r (a :- r)
param name (Boomerang valueBmg) =
  Boomerang
    { prs: prs
    , ser: ser
    }
 where
  prs :: ParserT Url Identity (r -> a :- r)
  prs =
    ParserT prs'
   where
    prs' (PState t) =
      case pop name input.query of
        Just (Tuple maybeValue query') ->
          case maybeValue of
            Nothing ->
              pure $ parseFailed input t.position ("Param required: " <> name <> ".")
            Just value -> do
              case parseURL value of
                Nothing -> pure $ parseFailed input t.position ("Incorrect uri encoded in param: " <> name <> ".")
                Just url ->
                  let ev = runParser url valueBmg.prs
                  in case ev of
                    Left e -> pure $ parseFailed input t.position ("Fail to parse param: " <> name <> ".")
                    Right v -> pure
                      { input: (input {query = query'})
                      , result: Right v
                      , consumed: false
                      , position: t.position
                      }
        Nothing -> pure $ parseFailed input t.position ("Mising param: " <> name <> ".")
     where
      input = t.input

  ser :: Serializer Url (a :- r) r
  ser =
    Serializer $ \v -> case runSerializer valueBmg.ser v of
      Just (Tuple f rest) ->
        let url = f { path: "", query: empty }
            v' =
              unsafePartial $
                case printURL url of
                  Just s -> s
        in pure (Tuple (\r -> r { query = insert name (Just v') r.query}) rest)
      Nothing -> Nothing

string' :: forall a. String -> UrlBoomerang a (String :- a)
string' s = liftStringBoomerang (string s)

boolean :: forall r. UrlBoomerang r (Boolean :- r)
boolean =
  liftStringBoomerang $ boolean' <<< (string "on" <> string "off")
 where
  boolean' :: forall s. StringBoomerang (String :- s) (Boolean :- s)
  boolean' =
    maph prs ser
   where
    prs "on" = true
    prs _    = false

    ser true  = Just "on"
    ser false = Just "off"

foreign import encodeURIComponent :: String -> String
foreign import decodeURIComponent :: String -> String

str :: forall r. UrlBoomerang r (String :- r)
str =
  liftStringBoomerang $ maph (decodeURIComponent) (Just <<< encodeURIComponent) <<< many1NoneOf "/?#"

int :: forall r. UrlBoomerang r (Int :- r)
int = liftStringBoomerang $ Text.Boomerang.String.int

