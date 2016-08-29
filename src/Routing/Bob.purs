module Routing.Bob where

import Prelude
import Data.Array as Data.Array
import Control.Error.Util (hush)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Array (uncons)
import Data.Foldable (class Foldable, foldr, foldlDefault, foldrDefault, fold)
import Data.Generic (class Generic, GenericSignature(SigString, SigBoolean, SigInt, SigProd), GenericSpine(SString, SInt, SBoolean, SProd), toSpine, fromSpine, toSignature)
import Data.List (List(Cons, Nil), null, fromFoldable, concatMap, (:))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid (mempty)
import Data.NonEmpty (fromNonEmpty, NonEmpty, (:|), foldMap1)
import Data.StrMap (empty)
import Data.String (split)
import Data.Traversable (class Traversable, traverse, for)
import Data.Tuple (Tuple(Tuple))
import Partial.Unsafe (unsafePartial)
import Routing.Bob.UrlBoomerang (str, boolean, liftStringBoomerang, liftStringPrs, Url, UrlBoomerang)
import Text.Boomerang.Combinators (maph, nil, cons, duck1)
import Text.Boomerang.HStack (hSingleton, hNil, hHead, HNil, type (:-), (:-))
import Text.Boomerang.Prim (Boomerang(Boomerang), runSerializer)
import Text.Boomerang.String (int, lit)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (eof)
import Type.Proxy (Proxy(Proxy))

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

anaM :: forall a f m. (Functor f, Monad m, Traversable f) => (a -> m (f a)) -> a -> m (Fix f)
anaM coalgM = ((map Fix) <<< traverse (anaM coalgM)) <=< coalgM

-- subset of GenericSignature which is covered by this library
data SigF r
  = SigProdF String (NonEmpty List (DataConstructorF r))
  -- | SigRecordF (Array { recLabel :: String, recValue :: SigRecValue })
  | SigBooleanF
  | SigIntF
  | SigStringF

type DataConstructorF r = { sigConstructor :: String, sigValues :: List r}

data SigRecValue
  = SigRecInt
  | SigRecBoolean

instance functorSigF :: Functor SigF where
  map f (SigProdF s a) = SigProdF s (map (\r -> r { sigValues=map f r.sigValues }) a)
  -- map _ (SigRecordF a) = SigRecordF a
  map _ SigBooleanF = SigBooleanF
  map _ SigIntF = SigIntF
  map _ SigStringF = SigStringF

instance foldableSigF :: Foldable SigF where
  foldMap f (SigProdF s a) =
    fold <<< concatMap (\r -> map f r.sigValues) <<< (fromNonEmpty (:)) $ a
  foldMap _ _ = mempty
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance traversableSigF :: Traversable SigF where
  traverse f (SigProdF s a) =
    SigProdF s <$> for a (\r -> r { sigValues = _ } <$> for r.sigValues f)
  -- traverse _ (SigRecordF a) = pure (SigRecordF a)
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
fromGenericSignature SigInt = Just SigIntF
fromGenericSignature SigBoolean = Just SigBooleanF
fromGenericSignature SigString = Just SigStringF
fromGenericSignature _ = Nothing

type UrlBoomerangForGenericSpine r = UrlBoomerang r (GenericSpine :- r)

toSpineBoomerang :: forall r. SigF (UrlBoomerangForGenericSpine r) -> UrlBoomerangForGenericSpine r
toSpineBoomerang s@(SigProdF _ cs@(h :| t)) =
  foldMap1 fromConstructor cs
 where
  fromConstructor :: DataConstructorF (UrlBoomerangForGenericSpine r) -> UrlBoomerangForGenericSpine r
  fromConstructor constructor =
    bmg
   where
    valuesBmg =
      intersperce (liftStringBoomerang (lit "/")) <<< map (lazy <<< _) <<< _.sigValues $ constructor
     where
      lazy :: forall z y. UrlBoomerang (y :- z) ((Unit -> y) :- z)
      lazy = maph const (Just <<< (_ $ unit))

      intersperce :: forall tok a t. (forall r. Boomerang tok r r) ->
                                     List (Boomerang tok t (a :- t)) ->
                                     Boomerang tok t ((List a) :- t)
      intersperce _ Nil = nil
      intersperce sep (Cons b t) =
        cons <<< duck1 b <<< foldr step nil t
       where
        step e l = sep <<< cons <<< duck1 e <<< l

    constructorBmg =
      maph prs ser <<< arrayFromList <<< valuesBmg
     where
      arrayFromList :: forall t a tok. Boomerang tok (List a :- t) (Array a :- t)
      arrayFromList =
        maph arrayFromFoldable (Just <<< fromFoldable)
       where
        -- very ineficient - will be replaced with next purescript-array release
        arrayFromFoldable = foldr Data.Array.cons []

      prs = SProd constructor.sigConstructor
      ser (SProd c values) | c == constructor.sigConstructor = Just values
                           | otherwise = Nothing
      ser _                = Nothing

    constructorNameBmg =
      liftStringBoomerang (lit constructorName)
     where
      constructorName = serializeConstructorName constructor.sigConstructor

    bmg | null t = constructorBmg
        | null constructor.sigValues = (constructorNameBmg <<< constructorBmg)
        | otherwise = constructorNameBmg <<< liftStringBoomerang (lit "/") <<< constructorBmg
toSpineBoomerang SigBooleanF =
  liftStringBoomerang (maph SBoolean ser <<< boolean)
 where
  ser (SBoolean b) = Just b
  ser _            = Nothing
toSpineBoomerang SigIntF =
  liftStringBoomerang (maph SInt ser <<< int)
 where
  ser (SInt b) = Just b
  ser _        = Nothing
toSpineBoomerang SigStringF =
  liftStringBoomerang (maph SString ser <<< str)
 where
  ser (SString s) = Just s
  ser _        = Nothing
-- SigRecord (Array { recLabel :: String, recValue :: Unit -> GenericSignature }) =

parse :: forall a. UrlBoomerang HNil (a :- HNil) -> Url -> Maybe a
parse (Boomerang b) tok = do
  f <- hush (runParser tok (do
    r <- b.prs
    -- we have to consume whole input
    liftStringPrs eof
    pure r))
  pure (hHead (f hNil))

serialize :: forall a. UrlBoomerang HNil (a :- HNil) -> a -> Maybe Url
serialize (Boomerang b) s = do
  (Tuple f _) <- runSerializer b.ser (hSingleton s)
  pure (f { path: "", query: empty })

bob :: forall a r. (Generic a) => Proxy a -> Maybe (UrlBoomerang r (a :- r))
bob p = do
  sb <- cata toSpineBoomerang <$> (anaM fromGenericSignature (toSignature p))
  pure (maph prs (\v -> Just (Just v)) <<< maph fromSpine (toSpine <$> _) <<< sb)
 where
  prs (Just s) = s
  prs Nothing = unsafeThrow ("Incorrect spine generated for signature: " <> show (toSignature p))

foreign import camelsToHyphens :: String -> String

serializeConstructorName :: String -> String
serializeConstructorName n =
  camelsToHyphens (fromMaybe n (Data.Array.last <<< split "." $ n))

genericToUrl :: forall a. (Generic a) => a -> Maybe Url
genericToUrl a = do
  route <- bob (Proxy :: Proxy a)
  serialize route a

genericToUrlPath :: forall a. (Generic a) => a -> Maybe String
genericToUrlPath a = _.path <$> genericToUrl a

genericFromUrl :: forall a. (Generic a) => Url -> Maybe a
genericFromUrl u = do
  route <- bob (Proxy :: Proxy a)
  parse route u

genericFromUrlPath :: forall a. (Generic a) => String -> Maybe a
genericFromUrlPath s = genericFromUrl { path: s, query: empty }

data Router a = Router (UrlBoomerang HNil (a :- HNil))

router :: forall a. (Generic a) => Proxy a -> Maybe (Router a)
router p = do
  b <- bob p
  pure $ Router b

toUrl :: forall a. Router a -> a -> Url
toUrl (Router bmg) a =
  unsafePartial (case serialize bmg a of Just url -> url)

toUrlPath :: forall a. Router a -> a -> String
toUrlPath r a = let url = toUrl r a in url.path

fromUrl :: forall a. Router a -> Url -> Maybe a
fromUrl (Router bmg) url =
  parse bmg url

fromUrlPath :: forall a. Router a -> String -> Maybe a
fromUrlPath r s = fromUrl r { path: s, query: empty }
