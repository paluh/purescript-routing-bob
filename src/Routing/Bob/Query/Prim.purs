module Routing.Bob.Query.Prim where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.State.Trans (StateT(..), runStateT)
import Data.Either (Either(Left, Right))
import Data.List (List(Nil))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap)
import Data.StrMap (insert, pop)
import Data.Tuple (Tuple(Tuple))
import Routing.Bob.UrlBoomerang (Url(..), UrlBoomerang, UrlParser, UrlSerializer)
import Text.Boomerang.HStack (type (:-))
import Text.Boomerang.Prim (Boomerang(Boomerang), Parsers(..), Serializer(Serializer), runSerializer)
import Text.Parsing.Parser (ParseError, ParseState(ParseState), ParserT(ParserT), fail)
import Text.Parsing.Parser.Pos (Position)

type Key = String
type Value = List String
type ValueParser a b = Parsers Value (a -> b)
type ValueSerializer b a = Serializer Value b a
type ValueBoomerang a b = Boomerang Value a b



-- | XXX: This error/position handling mess will be fixed in next release

toQueryParser ::
  forall a r. Key -> ValueParser r (a :- r) -> UrlParser r (a :- r)
toQueryParser key valuePrs =
  Parsers <<< ParserT <<< ExceptT <<< StateT $ parser
 where
  parser ::
    ParseState Url ->
    List
      (Tuple (Either ParseError (r -> (a :- r))) (ParseState Url))
  parser (ParseState (Url i) position c) =
    let
      v = case pop key i.query of
        Nothing -> { value: Nil, query: i.query }
        Just (Tuple l query') -> { value: l, query: query' }
    in do
      Tuple e (ParseState path' p' c') <- runStateT (runExceptT (unwrap <<< unwrap $ valuePrs)) (ParseState v.value position false)
      let i' = i { query = v.query }
      pure (Tuple e (ParseState (Url i') position c'))

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
        in pure (Tuple (\(Url r) -> Url (r { query = insert' key value r.query})) rest)
      Nothing -> Nothing

  insert' _ Nil query = query
  insert' k v q = insert k v q

toQueryBoomerang ::
  forall a r. String -> ValueBoomerang r (a :- r) -> UrlBoomerang r (a :- r)
toQueryBoomerang key (Boomerang valueBmg) =
  Boomerang
    { prs: toQueryParser key valueBmg.prs
    , ser: toQuerySerializer key valueBmg.ser
    }


-- | XXX: Same problem with error handling is here as above

-- Text.Parsing.Parser.ParseError constructor is not exposed so...
parseFailed ::
  forall token m a. Monad m =>
  token ->
  Position ->
  String ->
  m (Tuple (Either ParseError a) (ParseState token))
parseFailed input position message =
  runStateT (runExceptT (unwrap (fail message))) (ParseState input position false)

-- | We allow arbitrary nesting of url encodings inside query parameters
-- | so you can put any type wich is generic inside record fields
-- | This is the reason why we are expecting UrlParser here ;-)

toValueParser ::
  forall a r.
    (Value -> Either String (r -> (a :- r))) ->
    ValueParser r (a :- r)
toValueParser parseValue =
  Parsers <<< ParserT <<< ExceptT <<< StateT $ parseFunction
 where
  parseFunction ::
    ParseState Value ->
    List
      (Tuple (Either ParseError (r -> (a :- r))) (ParseState Value))
  parseFunction (ParseState input position consumed) =
    case parseValue input of
      Left msg -> parseFailed input position msg
      (Right r) -> pure (Tuple (Right r) (ParseState Nil position true))

