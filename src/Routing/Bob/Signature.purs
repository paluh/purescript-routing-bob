module Routing.Bob.Signature where

import Prelude
import Data.Array (uncons)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault, fold)
import Data.Generic (GenericSignature(SigString, SigBoolean, SigInt, SigRecord, SigProd))
import Data.List (List, fromFoldable, concatMap, (:))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid (mempty)
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.Traversable (class Traversable, traverse, for)

-- subset of GenericSignature which is covered by this library
type DataConstructorF r = { sigConstructor :: String, sigValues :: List r}

data SigF r
  = SigProdF String (NonEmpty List (DataConstructorF r))
  | SigRecordF (NonEmpty List ({ recLabel :: String, recValue :: r }))
  | SigBooleanF
  | SigIntF
  | SigStringF

-- data SigRecValueF r
--   = SigRecSingletonValueF r
--   | SigRecOptionalValueF r
--   | SigRecArrayValueF r
--   | SigRecListValueF r

instance functorSigF :: Functor SigF where
  map f (SigProdF s l) = SigProdF s (map (\r -> r { sigValues = map f r.sigValues }) l)
  map f (SigRecordF l) = SigRecordF (map (\r -> r { recValue = f r.recValue }) l)
  map _ SigBooleanF = SigBooleanF
  map _ SigIntF = SigIntF
  map _ SigStringF = SigStringF

instance foldableSigF :: Foldable SigF where
  foldMap f (SigProdF s l) =
    fold <<< concatMap (\r -> map f r.sigValues) <<< (fromNonEmpty (:)) $ l
  foldMap f (SigRecordF l) =
    fold <<< map (\r -> f r.recValue) <<< (fromNonEmpty (:)) $ l
  foldMap _ _ = mempty
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance traversableSigF :: Traversable SigF where
  traverse f (SigProdF s l) =
    SigProdF s <$> for l (\r -> r { sigValues = _ } <$> for r.sigValues f)
  traverse f (SigRecordF l) =
    SigRecordF <$> for l (\r -> r { recValue = _ } <$> f r.recValue)
  traverse _ SigBooleanF = pure SigBooleanF
  traverse _ SigIntF = pure SigIntF
  traverse _ SigStringF = pure SigStringF
  sequence = traverse id

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
  fromField f = f { recValue = f.recValue unit }
fromGenericSignature SigInt = Just SigIntF
fromGenericSignature SigBoolean = Just SigBooleanF
fromGenericSignature SigString = Just SigStringF
fromGenericSignature _ = Nothing

