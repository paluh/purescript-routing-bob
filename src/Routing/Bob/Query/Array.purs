module Routing.Bob.Query.Array where

import Prelude

import Control.Error.Util (note)
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.EitherR (fmapL)
import Data.Generic (GenericSpine(SArray))
import Data.List (reverse)
import Data.List as Data.List
import Data.Maybe (Maybe(Nothing))
import Data.StrMap (empty)
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))
import Routing.Bob.Query.Prim (Value, ValueParser, ValueSerializer, ValueBoomerang, toValueParser)
import Routing.Bob.UrlBoomerang (Url(..), UrlBoomerang, UrlParser, UrlSerializer, parseURL, printURL)
import Text.Boomerang.HStack (type (:-), hHead, (:-))
import Text.Boomerang.Prim (Boomerang(Boomerang), Serializer(Serializer), parse1, runSerializer)
import Text.Parsing.Parser (parseErrorMessage)


toArrayParser ::
  forall r.
    UrlParser r (GenericSpine :- r) ->
    ValueParser r (GenericSpine :- r)
toArrayParser valuePrs =
  toValueParser parseValue
 where
  parseValue :: Value -> Either String (r -> (GenericSpine :- r))
  parseValue val = do
    urls <- note ("Some of values are incorrectly encoded: \"" <> show val <> "\"") (for val parseURL)
    fs <- fmapL parseErrorMessage (for urls (parse1 valuePrs))
    pure $ (\r -> (SArray <<< fromFoldable <<< reverse <<< map (const <<< hHead <<< (_ $ r)) $ fs) :- r)

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
        vs' <- for ts (printURL <<< (\(Tuple ser' _) -> ser' (Url { path: "", query: empty }) ))
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
