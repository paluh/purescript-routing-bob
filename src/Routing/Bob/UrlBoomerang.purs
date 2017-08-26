module Routing.Bob.UrlBoomerang where

import Prelude

import Control.Alt ((<|>))
import Control.Error.Util (hush)
import Control.Monad.Except.Trans (runExceptT, ExceptT(..))
import Control.Monad.State.Trans (runStateT, StateT(..))
import Data.Either (Either(Left, Right))
import Data.Identity (Identity)
import Data.List (singleton, List(Nil))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.StrMap (foldMap, fromFoldableWith, StrMap)
import Data.String (Pattern(..), stripSuffix)
import Data.Tuple (snd, Tuple(Tuple), fst)
import Data.URI (Query(Query), RelativePart(RelativePart), RelativeRef(RelativeRef))
import Data.URI.RelativePart as RelativePart
import Data.URI.RelativeRef as RelativeRef
import Text.Boomerang.Combinators (maph)
import Text.Boomerang.HStack (type (:-))
import Text.Boomerang.Prim (Boomerang(Boomerang), Serializer(Serializer))
import Text.Boomerang.String (many1NoneOf, string, StringBoomerang)
import Text.Boomerang.String as Text.Boomerang.String
import Text.Parsing.Parser (Parser, ParseState(ParseState), ParserT(ParserT))
import Text.Parsing.StringParser as StringParser

-- we want to parse/serialize query and path at the same time
type Url =
  { path :: String
  , query :: StrMap (List String)
  }

type UrlBoomerang a b = Boomerang Url a b
type UrlParser a b = ParserT Url Identity (a -> b)
type UrlSerializer a b = Serializer Url a b

-- | XXX: This error/position handling mess will be fixed in next release

parseURL :: String -> Maybe Url
parseURL s =
  case (StringParser.runParser RelativeRef.parser) s of
    Right (RelativeRef (RelativePart _ maybePath) maybeQuery _) ->
      let
        query =
          case maybeQuery of
            Nothing -> mempty
            (Just (Query q)) ->
              let toValue (Tuple k v) = maybe (Tuple k Nil) (Tuple k <<< singleton) v
              in fromFoldableWith (<>) <<< map toValue $ q
        path =
          case maybePath of
            Nothing -> ""
            Just p -> RelativePart.print (RelativePart Nothing maybePath)
      in
        Just
          { query: query
          , path: path
          }
    (Left _) -> Nothing

printURL :: Url -> Maybe String
printURL { path, query } = do
  relativePart <- hush $ StringParser.runParser RelativePart.parser path
  let url = RelativeRef.print (RelativeRef relativePart (Just (Query query')) Nothing)
  (stripSuffix (Pattern "?") url) <|> Just url
 where
  query' = foldMap toQueryValue query
  toQueryValue name Nil = singleton (Tuple name Nothing)
  toQueryValue name vs = map (Tuple name <<< Just) vs

-- XXX: quite hacky way to lift a parser
--      * position information is not accurate
--      * consume information is diregarded
liftStringPrs ::
  forall a e.
  Parser String a -> Parser { path :: String | e } a
liftStringPrs prs =
  ParserT <<< ExceptT <<< StateT $ parser
 where
  parser (ParseState i@{path} position c) = do
    Tuple e (ParseState path' p' _) <- runStateT (runExceptT (unwrap prs)) (ParseState path position false)
    let i' = i { path = path' }
    pure (Tuple e (ParseState i' p' c))

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

