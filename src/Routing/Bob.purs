module Routing.Bob where

import Prelude

import Control.Error.Util (hush)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Array as Data.Array
import Data.Foldable (foldr)
import Data.Generic (toSpine, class Generic, GenericSpine(SString, SInt, SBoolean, SRecord, SProd), fromSpine, toSignature)
import Data.List (List(Cons, Nil), null)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.NonEmpty (foldMap1, (:|))
import Data.StrMap (empty)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(Tuple))
import Partial.Unsafe (unsafePartial)
import Routing.Bob.Boomerangs (arrayFromList, lazy)
import Routing.Bob.Query.Array (toArrayBoomerag)
import Routing.Bob.Query.OptionalValue (toOptionalValueBoomerang)
import Routing.Bob.Query.Prim (toQueryBoomerang)
import Routing.Bob.Query.RequiredValue (toRequiredValueBoomerang)
import Routing.Bob.RecursionSchemes (anaM, cata)
import Routing.Bob.Signature (fromGenericSignature, SigRecValueF(SigRecArrayF, SigRecOptionalValueF, SigRecRequiredValueF), SigF(SigStringF, SigProdF, SigIntF, SigBooleanF, SigRecordF))
import Routing.Bob.UrlBoomerang (Url(Url), UrlBoomerang, boolean, int, liftStringBoomerang, parseURL, printURL, str)
import Text.Boomerang.Combinators (eof, maph, nil, duck1, cons)
import Text.Boomerang.HStack (type (:-), HNil, hHead, hNil, hSingleton)
import Text.Boomerang.Prim (Boomerang(Boomerang), parse1, runSerializer)
import Text.Boomerang.String (lit)
import Type.Proxy (Proxy(Proxy))

type UrlBoomerangForGenericSpine r = UrlBoomerang r (GenericSpine :- r)

toSpineBoomerang ::
  forall r.
    Options -> SigF (UrlBoomerangForGenericSpine r) -> UrlBoomerangForGenericSpine r
toSpineBoomerang _ (SigRecordF l) =
  -- add/drop record constructor
  maph SRecord ser <<<
  -- convert array <-> list
  arrayFromList <<<
  -- for every record field use appropriate boomerang with appropriate label
  -- and join this boomerangs into list
  fieldsBmg
 where
  step e r =
    cons <<< duck1 fieldBmg <<< r
   where
    fieldBmg =
      maph { recLabel: e.recLabel, recValue: _} (Just <<< _.recValue) <<< lazy <<< fromRecValue e.recLabel e.recValue

  ser (SRecord a) = Just a
  ser _ = Nothing

  fieldsBmg = foldr step nil l

  fromRecValue ::
    forall r'. String -> SigRecValueF (UrlBoomerangForGenericSpine r') -> UrlBoomerangForGenericSpine r'
  fromRecValue key (SigRecOptionalValueF just nothing valueBmg) =
    toQueryBoomerang key <<< toOptionalValueBoomerang just nothing $ valueBmg
  fromRecValue key (SigRecRequiredValueF valueBmg) =
    toQueryBoomerang key <<< toRequiredValueBoomerang $ valueBmg
  fromRecValue key (SigRecArrayF valueBmg) =
    toQueryBoomerang key <<< toArrayBoomerag $ valueBmg
toSpineBoomerang opts (SigProdF _ cs@(h :| t)) =
  foldMap1 fromConstructor cs
 where
  -- fromConstructor :: DataConstructorF (UrlBoomerangForGenericSpine r) -> UrlBoomerangForGenericSpine r
  fromConstructor constructor =
    bmg
   where
    valuesBmg =
      intersperce (liftStringBoomerang (lit "/")) <<< map (lazy <<< _) <<< _.sigValues $ constructor
     where
      intersperce :: forall tok a t. (forall r. Boomerang tok r r) ->
                                     List (Boomerang tok t (a :- t)) ->
                                     Boomerang tok t (List a :- t)
      intersperce _ Nil = nil
      intersperce sep (Cons b t') =
        cons <<< duck1 b <<< foldr step nil t'
       where
        step e l = sep <<< cons <<< duck1 e <<< l

    constructorBmg =
      maph prs ser <<< arrayFromList <<< valuesBmg
     where
      prs = SProd constructor.sigConstructor
      ser (SProd c values)
        | c == constructor.sigConstructor = Just values
        | otherwise = Nothing
      ser _ = Nothing

    constructorNameBmg =
      liftStringBoomerang <<< lit <<< opts.serializeConstructorName $ constructor.sigConstructor

    bmg
      | null t = constructorBmg
      | null constructor.sigValues = constructorNameBmg <<< constructorBmg
      | otherwise = constructorNameBmg <<< liftStringBoomerang (lit "/") <<< constructorBmg
toSpineBoomerang _ SigBooleanF =
  maph SBoolean ser <<< boolean
 where
  ser (SBoolean b) = Just b
  ser _            = Nothing
toSpineBoomerang _ SigIntF =
  maph SInt ser <<< int
 where
  ser (SInt b) = Just b
  ser _        = Nothing
toSpineBoomerang _ SigStringF =
  maph SString ser <<< str
 where
  ser (SString s) = Just s
  ser _        = Nothing

parse :: forall a. UrlBoomerang HNil (a :- HNil) -> Url -> Maybe a
parse (Boomerang b) tok = do
  f <- hush (parse1 (b.prs) tok)
  pure (hHead (f hNil))

serialize :: forall a. UrlBoomerang HNil (a :- HNil) -> a -> Maybe Url
serialize (Boomerang b) s = do
  (Tuple f _) <- runSerializer b.ser (hSingleton s)
  pure (f (Url { path: "", query: empty }))

bob :: forall a r. (Generic a) => Options -> Proxy a -> Maybe (UrlBoomerang r (a :- r))
bob opts p = do
  sb <- cata (toSpineBoomerang opts) <$> (anaM fromGenericSignature (toSignature p))
  pure (maph prs (\v -> Just (Just v)) <<< maph fromSpine (toSpine <$> _) <<< sb)
 where
  prs (Just s) = s
  prs Nothing = unsafeThrow ("Incorrect spine generated for signature: " <> show (toSignature p))

bob' :: forall a r. (Generic a) => Proxy a -> Maybe (UrlBoomerang r (a :- r))
bob' p = bob defaultOptions p

foreign import camelsToHyphens :: String -> String

genericToUrl :: forall a. (Generic a) => Options -> a -> Maybe String
genericToUrl opts a = do
  route <- bob opts (Proxy :: Proxy a)
  url <- serialize route a
  printURL url

genericToUrl' :: forall a. Generic a => a -> Maybe String
genericToUrl' = genericToUrl defaultOptions

genericFromUrl :: forall a. (Generic a) => Options -> String -> Maybe a
genericFromUrl opts s = do
  url <- parseURL s
  route <- bob opts (Proxy :: Proxy a)
  parse route url

genericFromUrl' :: forall a. Generic a => String -> Maybe a
genericFromUrl' = genericFromUrl defaultOptions

newtype Router a = Router (UrlBoomerang HNil (a :- HNil))
derive instance newtypeRouter :: Newtype (Router a) _

router :: forall a. (Generic a) => Options -> Proxy a -> Maybe (Router a)
router opts p = do
  b <- bob opts p
  pure $ Router (b <<< eof)

router' :: forall a. Generic a => Proxy a -> Maybe (Router a)
router' = router defaultOptions

toUrl :: forall a. Router a -> a -> String
toUrl (Router bmg) a =
  unsafePartial (case serialize bmg a >>= printURL of Just s -> s)

fromUrl :: forall a. Router a -> String -> Maybe a
fromUrl (Router bmg) url = do
  p <- parseURL url
  parse bmg p

type Options = {
  serializeConstructorName :: String -> String
}

defaultOptions :: { serializeConstructorName ∷ String -> String }
defaultOptions = {
  serializeConstructorName: \n -> camelsToHyphens (fromMaybe n (Data.Array.last <<< split (Pattern ".") $ n))
}

-- data Urls a = Root | Route a
-- derive instance genericRepUrls :: Generic.Rep.Generic (Urls a) _
--
-- routeB :: forall a r t
--   . (Generic.Rep.Generic a t)
--   ⇒ UrlBoomerang r (a :- r)
--   -> UrlBoomerang r (Urls a :- r)
-- routeB r = constructorBoomerang (SProxy :: SProxy "Route") <<< liftStringBoomerang (lit "/") <<< r
-- rootB :: forall a r. UrlBoomerang r (Urls a :- r)
-- rootB = constructorBoomerang (SProxy :: SProxy "Root") <<< liftStringBoomerang (lit "/")
--
-- urlsR :: forall a r t
--   . (Generic.Rep.Generic a t)
--   ⇒ Router a
--   -> Router (Urls a)
-- urlsR r = Router (routeB (unwrap r) <> rootB)

-- route :: forall a t. (Generic.Rep.Generic a t) ⇒ Router a -> String -> Maybe (Urls a)
-- route r u = fromUrl (urlsR r)
