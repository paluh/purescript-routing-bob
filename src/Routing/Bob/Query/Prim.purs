module Routing.Bob.Query.Prim where

import Prelude
import Data.Either (Either(Left))
import Data.Identity (Identity)
import Data.List (List(Nil))
import Data.Maybe (Maybe(Nothing, Just))
import Data.NonEmpty ((:|))
import Data.StrMap (insert, pop)
import Data.Tuple (Tuple(Tuple))
import Routing.Bob.UrlBoomerang (UrlBoomerang, UrlSerializer, UrlParser)
import Text.Boomerang.HStack (type (:-))
import Text.Boomerang.Prim (Serializer(Serializer), runSerializer, Boomerang(Boomerang))
import Text.Parsing.Parser (PState(PState), ParseError(ParseError), ParserT(ParserT), parseFailed, runParser)

type Key = String
type Value = List String
type ValueParser a b = ParserT Value Identity (a -> b)
type ValueSerializer b a = Serializer Value b a
type ValueBoomerang a b = Boomerang Value a b

toQueryParser ::
  forall a r. Key -> ValueParser r (a :- r) -> UrlParser r (a :- r)
toQueryParser key valuePrs =
  ParserT (pure <$> prs)
 where
  prs (PState s) =
    let
      v = case pop key s.input.query of
        Nothing -> { value: Nil, query: s.input.query }
        Just (Tuple l query') -> { value: l, query: query' }
      parseResult = runParser v.value valuePrs
    in case parseResult of
      Left (ParseError p) -> parseFailed s.input s.position p.message
      result -> { input: s.input { query = v.query }, result, consumed: true, position: s.position }

toQuerySerializer ::
  forall a r. Key -> ValueSerializer (a :- r) r -> UrlSerializer (a :- r) r
toQuerySerializer key valueSer =
  ser
 where
  ser = Serializer $ \v ->
    case runSerializer valueSer v of
      Just (Tuple sf rest) ->
        let
          value = sf Nil
        in pure (Tuple (\r -> r { query = insert' key value r.query}) rest)
      Nothing -> Nothing

  insert' _ Nil query = query
  insert' key value query = insert key value query

toQueryBoomerang ::
  forall a r. String -> ValueBoomerang r (a :- r) -> UrlBoomerang r (a :- r)
toQueryBoomerang key (Boomerang valueBmg) =
  Boomerang
    { prs: toQueryParser key valueBmg.prs
    , ser: toQuerySerializer key valueBmg.ser
    }

