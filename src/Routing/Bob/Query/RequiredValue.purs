module Routing.Bob.Query.RequiredValue where

import Prelude

import Control.Error.Util (note)
import Data.Either (Either(Left))
import Data.EitherR (fmapL)
import Data.List (List(Nil), (:))
import Data.StrMap (empty)
import Data.Tuple (Tuple(Tuple))
import Routing.Bob.Query.Prim (ValueParser, ValueSerializer, ValueBoomerang, Value, toValueParser)
import Routing.Bob.UrlBoomerang (Url(..), UrlBoomerang, UrlParser, UrlSerializer, parseURL, printURL)
import Text.Boomerang.HStack (type (:-), (:-))
import Text.Boomerang.Prim (Boomerang(Boomerang), Serializer(Serializer), parse1, runSerializer)
import Text.Parsing.Parser (parseErrorMessage)

toRequiredValueParser :: forall a r. UrlParser r (a :- r) -> ValueParser r (a :- r)
toRequiredValueParser urlPrs =
  toValueParser parseValue
 where
  parseValue :: Value -> Either String (r -> (a :- r))
  parseValue val = case val of
    Nil -> Left "Missing value"
    (v : Nil) -> do
      url <- note ("Incorrect uri encoded in query param value: \"" <> v <> "\"") (parseURL v)
      fmapL parseErrorMessage <<< parse1 urlPrs $ url
    _ -> Left "Multiple values but expecting singleton"

toRequiredValueSerializer ::
  forall a r.
    UrlSerializer (a :- r) r ->
    ValueSerializer (a :- r) r
toRequiredValueSerializer valueSer =
  Serializer ser
 where
  ser (a :- r) = do
    (Tuple ser' r')  <- runSerializer valueSer (a :- r)
    v' <- printURL $ ser' (Url {path: "", query: empty})
    pure (Tuple (v' : _) r')

toRequiredValueBoomerang ::
  forall a r.
    UrlBoomerang r (a :- r) ->
    ValueBoomerang r (a :- r)
toRequiredValueBoomerang (Boomerang valueBmg) =
  Boomerang
    { prs: toRequiredValueParser valueBmg.prs
    , ser: toRequiredValueSerializer valueBmg.ser
    }

