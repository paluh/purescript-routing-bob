module Routing.Bob.Signature where

import Prelude
import Data.Array (uncons)
import Data.Foldable (foldMap, class Foldable, foldlDefault, foldrDefault, fold)
import Data.Generic (GenericSignature(SigArray, SigString, SigBoolean, SigInt, SigRecord, SigProd))
import Data.List (List, fromFoldable, concatMap, (:))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid (mempty)
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.Traversable (class Traversable, traverse, for)

-- subset of GenericSignature with
-- additional constructs in case of record fields - SigRecValueF
-- which are covered by this library
type DataConstructorF r = { sigConstructor :: String, sigValues :: List r}

data SigF r
  = SigProdF String (NonEmpty List (DataConstructorF r))
  | SigRecordF (NonEmpty List ({ recLabel :: String, recValue :: (SigRecValueF r) }))
  | SigBooleanF
  | SigIntF
  | SigStringF

type JustConstructorName = String
type NothingConstrtuctorName = String

data SigRecValueF r
  = SigRecRequiredValueF r
  | SigRecOptionalValueF JustConstructorName NothingConstrtuctorName r
  | SigRecArrayF r
--  | SigRecListValueF ConsConstructorName NilConstructorName r

instance functorSigRecValueF :: Functor SigRecValueF where
  map f (SigRecRequiredValueF r) = SigRecRequiredValueF (f r)
  map f (SigRecOptionalValueF j n r) = SigRecOptionalValueF j n (f r)
  map f (SigRecArrayF r) = SigRecArrayF (f r)

instance functorSigF :: Functor SigF where
  map f (SigProdF s l) = SigProdF s (map (\r -> r { sigValues = map f r.sigValues }) l)
  map f (SigRecordF l) = SigRecordF (map (\r -> r { recValue = map f r.recValue }) l)
  map _ SigBooleanF = SigBooleanF
  map _ SigIntF = SigIntF
  map _ SigStringF = SigStringF

instance foldableSigRecValueF :: Foldable SigRecValueF where
  foldMap f (SigRecRequiredValueF r) = f r
  foldMap f (SigRecOptionalValueF _ _ r) = f r
  foldMap f (SigRecArrayF r) = f r
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance foldableSigF :: Foldable SigF where
  foldMap f (SigProdF s l) =
    fold <<< concatMap (\r -> map f r.sigValues) <<< (fromNonEmpty (:)) $ l
  foldMap f (SigRecordF l) =
    fold <<< map (\r -> foldMap f r.recValue) <<< (fromNonEmpty (:)) $ l
  foldMap _ _ = mempty
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance traversableSigRecValueF :: Traversable SigRecValueF where
  traverse f (SigRecRequiredValueF r) = SigRecRequiredValueF <$> (f r)
  traverse f (SigRecOptionalValueF j n r) = (SigRecOptionalValueF j n) <$> (f r)
  traverse f (SigRecArrayF r) = SigRecArrayF <$> (f r)
  sequence = traverse id

instance traversableSigF :: Traversable SigF where
  traverse f (SigProdF s l) =
    SigProdF s <$> for l (\r -> r { sigValues = _ } <$> for r.sigValues f)
  traverse f (SigRecordF l) =
    SigRecordF <$> for l (\r -> r { recValue = _ } <$> traverse f r.recValue)
  traverse _ SigBooleanF = pure SigBooleanF
  traverse _ SigIntF = pure SigIntF
  traverse _ SigStringF = pure SigStringF
  sequence = traverse id

type OptionalMatch =
  { nothing :: String
  , just :: String
  , value :: GenericSignature
  }

matchOptional :: GenericSignature -> Maybe OptionalMatch
matchOptional (SigProd _ [ j@{ sigValues: [v] }, n@{ sigValues: [] }]) =
  Just { just: j.sigConstructor, nothing: n.sigConstructor, value: v unit}
matchOptional (SigProd _ [ n@{ sigValues: [] }, j@{ sigValues: [v] }]) =
  Just { just: j.sigConstructor, nothing: n.sigConstructor, value: v unit}
matchOptional _ = Nothing

fromGenericSignature :: GenericSignature -> Maybe (SigF GenericSignature)
fromGenericSignature (SigProd s a) = do
  {head: h, tail: t} <- uncons <<< map fromConstructor $ a
  pure $ SigProdF s (h :| fromFoldable t)
 where
  fromConstructor c = c { sigValues = fromFoldable $ map (_ $ unit) c.sigValues }
fromGenericSignature (SigRecord a) = do
  {head: h, tail: t} <- uncons <<< map fromField $ a
  pure $ SigRecordF (h :| fromFoldable t)
 where
  fromField f = f { recValue = recValue <<< f.recValue $ unit }
  recValue (SigArray v) =
    SigRecArrayF (v unit)
  recValue r =
    case matchOptional r of
      Just optionalMatch ->
        SigRecOptionalValueF optionalMatch.just optionalMatch.nothing optionalMatch.value
      Nothing -> SigRecRequiredValueF r
fromGenericSignature SigInt = Just SigIntF
fromGenericSignature SigBoolean = Just SigBooleanF
fromGenericSignature SigString = Just SigStringF
fromGenericSignature _ = Nothing

