module Routing.Bob.UrlBoomerang where

import Prelude

import Control.Alt ((<|>))
import Control.Error.Util (hush)
import Control.Monad.Except.Trans (runExceptT, ExceptT(..))
import Control.Monad.State.Trans (runStateT, StateT(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.List (singleton, List(Nil))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.StrMap (foldMap, fromFoldableWith, StrMap)
import Data.String (Pattern(..), stripSuffix)
import Data.Tuple (snd, Tuple(Tuple), fst)
import Data.URI (AbsoluteURI(..), HierarchicalPart(..), Path(..), Query(Query), RelativePart(RelativePart), RelativeRef(RelativeRef), URI(..))
import Data.URI.AbsoluteURI as AbsoluteURI
import Data.URI.Path (parsePathAbsolute, parsePathNoScheme, printPath)
import Data.URI.RelativePart as RelativePart
import Data.URI.RelativeRef as RelativeRef
import Data.URI.URI as URI
import Text.Boomerang.Combinators (maph)
import Text.Boomerang.HStack (type (:-))
import Text.Boomerang.Prim (Boomerang(Boomerang), Parsers(..), Serializer(Serializer))
import Text.Boomerang.String (many1NoneOf, string, StringBoomerang)
import Text.Boomerang.String as Text.Boomerang.String
import Text.Parsing.Parser (ParseState(ParseState), ParserT(ParserT))
import Text.Parsing.StringParser (runParser)
import Text.Parsing.StringParser as StringParser

-- we want to parse/serialize query and path at the same time
newtype Url = Url
  { path :: String
  , query :: StrMap (List String)
  }
derive instance genericRepUrl :: Generic Url _
derive instance newtypeUrl :: Newtype Url _
instance eqUrl :: Eq Url where
  eq = genericEq
instance semigroupUrl :: Semigroup Url where
  append u1 u2 =
    let
      u1' = unwrap u1
      u2' = unwrap u2
    in
      Url
        { path: u1'.path <> u2'.path
        , query: u1'.query <> u2'.query
        }
instance monoidUrl :: Monoid Url where
  mempty = Url { path: mempty, query: mempty }

type UrlBoomerang a b = Boomerang Url a b
type UrlParser a b = Parsers Url (a -> b)
type UrlSerializer a b = Serializer Url a b

-- | XXX: This error/position handling mess will be fixed in next release

parseURL :: String -> Maybe Url
parseURL s =
  case StringParser.runParser RelativeRef.parser s of
    Right (RelativeRef (RelativePart Nothing maybePath) maybeQuery Nothing) ->
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
            Just p -> printPath p
      in
        Just <<< wrap $
          { query: query
          , path: path
          }
    _ -> Nothing

printURL :: Url -> Maybe String
printURL (Url { path, query }) = do
  let pathParser = parsePathAbsolute <|> parsePathNoScheme
      -- XXX: probably we should handle here parsing errors...
      --      and reraise them loudly
      path' = hush (runParser pathParser path)
  let
    p = RelativePart Nothing path'
    q =  Just (Query query')
  let url = RelativeRef.print (RelativeRef p q Nothing)
  (stripSuffix (Pattern "?") url) <|> Just url
 where
  query' = foldMap toQueryValue query
  toQueryValue name Nil = singleton (Tuple name Nothing)
  toQueryValue name vs = map (Tuple name <<< Just) vs

-- XXX: quite hacky way to lift a parser
--      * position information is not accurate
--      * consume information is diregarded
liftStringPrs ::
  forall a.
  Parsers String a -> Parsers Url a
liftStringPrs prs =
  Parsers <<< ParserT <<< ExceptT <<< StateT $ parser
 where
  parser (ParseState (Url i@{path}) position c) = do
    Tuple e (ParseState path' p' _) <- runStateT (runExceptT (unwrap <<< unwrap $ prs)) (ParseState path position false)
    let i' = Url (i { path = path' })
    pure (Tuple e (ParseState i' p' c))

liftStringSer :: forall s s'. Serializer String s s' -> Serializer Url s s'
liftStringSer (Serializer ser) =
  Serializer ser'
 where
  ser' a = do
    t <- ser a
    let
      f = fst t
      f' (Url r) = Url (r { path = f r.path })
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

