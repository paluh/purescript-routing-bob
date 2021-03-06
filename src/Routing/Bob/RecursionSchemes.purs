module Routing.Bob.RecursionSchemes where

import Prelude
import Data.Traversable (class Traversable, traverse)

newtype Fix f = Fix (f (Fix f))

-- convertion between types and urls is done through
-- basic recursion schemes - cata and ana
-- don't be afraid it's nothing really fancy - it's just fold and unfold
-- but this strategy simplifies code a lot
unFix :: forall f. Fix f -> f (Fix f)
unFix (Fix f) = f

cata :: forall a f. Functor f => (f a -> a) -> Fix f -> a
cata alg = alg <<< (cata alg <$> _) <<< unFix

ana :: forall a f. Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix <<< (ana coalg <$> _) <<< coalg

anaM :: forall a f m. Functor f => Monad m => Traversable f => (a -> m (f a)) -> a -> m (Fix f)
anaM coalgM = ((map Fix) <<< traverse (anaM coalgM)) <=< coalgM
