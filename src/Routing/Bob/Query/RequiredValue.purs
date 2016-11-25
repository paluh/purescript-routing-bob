module Routing.Bob.Query.RequiredValue where

import Prelude
import Control.Error.Util (note)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(Left))
import Data.EitherR (fmapL)
import Data.List (List(Nil), length, (:))
import Data.NonEmpty ((:|))
import Data.StrMap (empty)
import Data.Tuple (Tuple(Tuple))
import Routing.Bob.Query.Prim (ValueParser, ValueSerializer, ValueBoomerang, Value, toValueParser)
import Routing.Bob.UrlBoomerang (UrlBoomerang, UrlSerializer, UrlParser, printURL, parseURL)
import Text.Boomerang.HStack (type (:-), (:-))
import Text.Boomerang.Prim (Serializer(Serializer), runSerializer, Boomerang(Boomerang))
import Text.Parsing.Parser (parseErrorMessage, runParser)
import Text.Parsing.Parser.Pos (Position(Position))

toRequiredValueParser :: forall a r. UrlParser r (a :- r) -> ValueParser r (a :- r)
toRequiredValueParser urlPrs =
  toValueParser parseValue
 where
  parseValue :: Value -> Either String (r -> (a :- r))
  parseValue val = case val of
    Nil -> Left "Missing value"
    (v : Nil) -> do
      url <- note ("Incorrect uri encoded in query param value: \"" <> v <> "\"") (parseURL v)
      fmapL parseErrorMessage (runParser url urlPrs)
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

