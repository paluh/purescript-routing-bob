module Routing.Bob.Boomerangs where

import Prelude
import Data.Array as Data.Array
import Data.List as Data.List
import Data.Foldable (foldr)
import Data.List (List)
import Data.Maybe (Maybe(Just))
import Data.Monoid (mempty, class Monoid)
import Data.Tuple (Tuple(Tuple))
import Text.Boomerang.Combinators (maph)
import Text.Boomerang.HStack (HNil(HNil), type (:-), (:-))
import Text.Boomerang.Prim (runSerializer, Boomerang(Boomerang))


lazy :: forall a r tok.  Boomerang tok (a :- r) ((Unit -> a) :- r)
lazy = maph const (Just <<< (_ $ unit))

arrayFromList :: forall a r tok. Boomerang tok (List a :- r) (Array a :- r)
arrayFromList =
  maph arrayFromFoldable (Just <<< Data.List.fromFoldable)
 where
  -- very ineficient - will be replaced with next purescript-array release
  arrayFromFoldable = foldr Data.Array.cons []

serialize :: forall a tok. (Monoid tok) => (forall r. Boomerang tok r (a :- r)) -> a -> Maybe tok
serialize (Boomerang b) s = do
  (Tuple f _) <- runSerializer b.ser (s :- HNil)
  pure (f mempty)
