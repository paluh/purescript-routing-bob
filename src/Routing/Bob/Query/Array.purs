module Routing.Bob.Query.Array where

import Prelude
import Data.List as Data.List
import Control.Error.Util (note)
import Data.Array (fromFoldable)
import Data.Either (Either(Left))
import Data.Generic (GenericSpine(SArray))
import Data.List (List(Nil), length, reverse, (:))
import Data.Maybe (Maybe(Nothing))
import Data.NonEmpty ((:|))
import Data.StrMap (empty)
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))
import Routing.Bob.UrlBoomerang (UrlSerializer, Url, UrlBoomerang, UrlParser, parseURL, printURL)
import Routing.Bob.Query.Prim (ValueParser, ValueSerializer, ValueBoomerang)
import Text.Boomerang.HStack (type (:-), hHead, (:-))
import Text.Boomerang.Prim (Serializer(Serializer), runSerializer, Boomerang(Boomerang))
import Text.Parsing.Parser (PState(PState), ParseError(ParseError), ParserT(ParserT), parseFailed, runParser)
import Text.Parsing.Parser.Pos (Position(Position))

toArrayParser ::
  forall r.
    UrlParser r (GenericSpine :- r) ->
    ValueParser r (GenericSpine :- r)
toArrayParser valuePrs =
  ParserT (pure <$> prs)
 where
  prs (PState s) =
    let
      result = case s.input of
        vs -> do
          urls <- note (parseError ("Some of values are incorrectly encoded: \"" <> show vs <> "\"") 1) (for vs parseURL)
          fs <- for urls (flip runParser valuePrs)
          pure $ (\r -> (SArray <<< fromFoldable <<< reverse <<< map (const <<< hHead <<< (_ $ r)) $ fs) :- r)
    in case result of
      Left (ParseError p) -> parseFailed s.input s.position p.message
      _ -> { input: Nil, result, consumed: true, position: position (length s.input) }
   where
    parseError message column =
      ParseError { message: message, position: Position {column: column, line: 1}}
    position column = Position { column, line: 1}

toArraySerializer ::
  forall r.
    UrlSerializer (GenericSpine :- r) r ->
    ValueSerializer (GenericSpine :- r) r
toArraySerializer valueSer =
  Serializer ser
 where
  ser (b :- r) =
    case b of
      SArray vs -> do
        ts <- for vs (runSerializer valueSer <<< (_ :- r) <<< (_ $ unit))
        vs' <- for ts (printURL <<< (\(Tuple ser' _) -> ser' ({ path: "", query: empty } :: Url) ))
        pure (Tuple (Data.List.fromFoldable vs' <> _) r)
      _ -> Nothing

toArrayBoomerag ::
  forall r.
    UrlBoomerang r (GenericSpine :- r) ->
    ValueBoomerang r (GenericSpine :- r)
toArrayBoomerag (Boomerang valueBmg) =
  Boomerang
    { prs: toArrayParser valueBmg.prs
    , ser: toArraySerializer valueBmg.ser
    }
