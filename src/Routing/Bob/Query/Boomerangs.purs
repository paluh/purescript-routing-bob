module Routing.Bob.Query.Boomerangs where

import Prelude
import Control.Error.Util (note)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(Right, Left))
import Data.Generic (toSpine, class Generic, fromSpine, GenericSpine(SProd))
import Data.Identity (Identity(Identity))
import Data.List (length, concat, singleton, List(Nil), (:))
import Data.Maybe (fromJust, Maybe(Nothing, Just))
import Data.Monoid (mempty, class Monoid)
import Data.NonEmpty ((:|))
import Data.StrMap (insert, pop, empty)
import Data.Tuple (Tuple(Tuple))
import Partial.Unsafe (unsafePartial)
import Routing.Bob.Boomerangs (serialize, lazy)
import Routing.Bob.RecursionSchemes (para, Fix(Fix))
import Routing.Bob.Signature (JustConstructorName, NothingConstrtuctorName, SigF(SigProdF))
import Routing.Bob.UrlBoomerang (UrlSerializer, Url, UrlBoomerang, UrlParser, parseURL, printURL)
import Text.Boomerang.Combinators (pureBmg, maph)
import Text.Boomerang.HStack (HNil(HNil), type (:-), (:-))
import Text.Boomerang.Prim (Serializer(Serializer), runSerializer, Boomerang(Boomerang))
import Text.Parsing.Parser (parseFailed, fail, runParser, ParseError(ParseError), PState(PState), ParserT(ParserT), Parser)
import Text.Parsing.Parser.Pos (Position(Position))

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

toSimpleValueParser ::
  forall a r.
    UrlParser r (a :- r) ->
    ValueParser r (a :- r)
toSimpleValueParser valuePrs =
  ParserT (pure <$> prs)
 where
  prs ::
    PState Value ->
    { input :: Value
    , result :: Either ParseError (r -> a :- r)
    , consumed :: Boolean
    , position :: Position }
  prs (PState s) =
    let
      result = case s.input of
        Nil -> throwError $ parseError ("Missing value" ) 1
        (v : Nil) -> do
          url <- note (parseError ("Incorrect uri encoded in query param value: \"" <> v <> "\"") 1) (parseURL v)
          runParser url valuePrs
        _ ->
          throwError (parseError "Multiple values but expecting singleton" 2)
    in case result of
      Left (ParseError p) -> parseFailed s.input s.position p.message
      _ -> { input: Nil, result, consumed: true, position: position (length s.input) }
   where
    parseError message column =
      ParseError { message: message, position: Position {column: column, line: 1}}
    -- XXX: for now simpified position handling
    position column = Position { column, line: 1}

toSimpleValueSerializer ::
  forall a r.
    UrlSerializer (a :- r) r ->
    ValueSerializer (a :- r) r
toSimpleValueSerializer valueSer =
  Serializer ser
 where
  ser (a :- r) = do
    (Tuple ser' r')  <- runSerializer valueSer (a :- r)
    v' <- printURL $ ser' {path: "", query: empty}
    pure (Tuple (v' : _) r')

toSimpleValueBoomerang ::
  forall a r.
    UrlBoomerang r (a :- r) ->
    ValueBoomerang r (a :- r)
toSimpleValueBoomerang (Boomerang valueBmg) =
  Boomerang
    { prs: toSimpleValueParser valueBmg.prs
    , ser: toSimpleValueSerializer valueBmg.ser
    }

toOptionalValueParser ::
  forall r.
    JustConstructorName ->
    NothingConstrtuctorName ->
    UrlParser r (GenericSpine :- r) ->
    ValueParser r (GenericSpine :- r)
toOptionalValueParser just nothing valuePrs =
  ParserT (pure <$> prs)
 where
  prs ::
    PState Value ->
    { input :: Value
    , result :: Either ParseError (r -> GenericSpine :- r)
    , consumed :: Boolean
    , position :: Position }
  prs (PState s) =
    let
      result = case s.input of
        Nil -> do
          pure (SProd nothing [] :- _)
        (v : Nil) -> do
          url <- note (parseError ("Incorrect uri encoded in query param value: \"" <> v <> "\"") 1) (parseURL v)
          prs' <- runParser url valuePrs
          pure
            (\r -> case prs' r of
              valueSpine :- r' -> (SProd just [\_ -> valueSpine] :- r'))
        _ ->
          throwError (parseError "Maybe iso parser can handle only single value" 2)
    in case result of
      Left (ParseError p) -> parseFailed s.input s.position p.message
      result' -> { input: Nil, result: result', consumed: true, position: position (length s.input) }
   where
    parseError message column =
      ParseError { message: message, position: Position {column: column, line: 1}}
    -- XXX: for now simpified position handling
    position column = Position { column, line: 1}

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
        v' <- printURL $ ser' {path: "", query: empty}
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
