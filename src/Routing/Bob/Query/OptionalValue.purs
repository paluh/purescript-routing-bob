module Routing.Bob.Query.OptionalValue where

import Prelude

import Control.Error.Util (note)
import Data.Either (Either(Left))
import Data.EitherR (fmapL)
import Data.Generic (GenericSpine(SProd))
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(Nothing, Just))
import Data.StrMap (empty)
import Data.Tuple (Tuple(Tuple))
import Routing.Bob.Query.Prim (ValueParser, ValueSerializer, ValueBoomerang, Value, toValueParser)
import Routing.Bob.Signature (NothingConstrtuctorName, JustConstructorName)
import Routing.Bob.UrlBoomerang (Url(..), UrlBoomerang, UrlParser, UrlSerializer, parseURL, printURL)
import Text.Boomerang.HStack (type (:-), (:-))
import Text.Boomerang.Prim (Serializer(Serializer), runSerializer, Boomerang(Boomerang), parse1)
import Text.Parsing.Parser (parseErrorMessage)

toOptionalValueParser ::
  forall r.
    JustConstructorName ->
    NothingConstrtuctorName ->
    UrlParser r (GenericSpine :- r) ->
    ValueParser r (GenericSpine :- r)
toOptionalValueParser just nothing valuePrs =
  toValueParser parseValue
 where
  parseValue :: Value -> Either String (r -> (GenericSpine :- r))
  parseValue val = case val of
    Nil -> pure (SProd nothing [] :- _)
    (v : Nil) -> do
      url <- note ("Incorrect uri encoded in query param value: \"" <> v <> "\"") (parseURL v)
      prs' <- fmapL parseErrorMessage <<< parse1 valuePrs $ url
      pure
        (\r -> case prs' r of
          valueSpine :- r' -> (SProd just [\_ -> valueSpine] :- r'))
    _ -> Left "Maybe iso parser can handle only single value"

toOptionalValueSerializer ::
  forall r.
    JustConstructorName ->
    NothingConstrtuctorName ->
    UrlSerializer (GenericSpine :- r) r ->
    ValueSerializer (GenericSpine :- r) r
toOptionalValueSerializer just nothing valueSer =
  Serializer ser
 where
  ser (b :- r) =
    case b of
      SProd n [] | n == nothing -> Just (Tuple id r)
      SProd j [v] | j == just -> do
        (Tuple ser' r')  <- runSerializer valueSer (v unit :- r)
        v' <- printURL $ ser' (Url {path: "", query: empty})
        pure (Tuple (v' : _) r')
      _ -> Nothing

toOptionalValueBoomerang ::
  forall r.
    JustConstructorName ->
    NothingConstrtuctorName ->
    UrlBoomerang r (GenericSpine :- r) ->
    ValueBoomerang r (GenericSpine :- r)
toOptionalValueBoomerang just nothing (Boomerang valueBmg) =
  Boomerang
    { prs: toOptionalValueParser just nothing valueBmg.prs
    , ser: toOptionalValueSerializer just nothing valueBmg.ser
    }

