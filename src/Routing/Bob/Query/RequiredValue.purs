module Routing.Bob.Query.RequiredValue where

import Prelude
import Control.Error.Util (note)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(Left))
import Data.List (List(Nil), length, (:))
import Data.NonEmpty ((:|))
import Data.StrMap (empty)
import Data.Tuple (Tuple(Tuple))
import Routing.Bob.Query.Prim (ValueParser, ValueSerializer, ValueBoomerang, Value)
import Routing.Bob.UrlBoomerang (UrlBoomerang, UrlSerializer, UrlParser, printURL, parseURL)
import Text.Boomerang.HStack (type (:-), (:-))
import Text.Boomerang.Prim (Serializer(Serializer), runSerializer, Boomerang(Boomerang))
import Text.Parsing.Parser (PState(PState), ParseError(ParseError), ParserT(ParserT), parseFailed, runParser)
import Text.Parsing.Parser.Pos (Position(Position))

toRequiredValueParser ::
  forall a r.
    UrlParser r (a :- r) ->
    ValueParser r (a :- r)
toRequiredValueParser valuePrs =
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

toRequiredValueSerializer ::
  forall a r.
    UrlSerializer (a :- r) r ->
    ValueSerializer (a :- r) r
toRequiredValueSerializer valueSer =
  Serializer ser
 where
  ser (a :- r) = do
    (Tuple ser' r')  <- runSerializer valueSer (a :- r)
    v' <- printURL $ ser' {path: "", query: empty}
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

