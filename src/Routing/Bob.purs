module Routing.Bob where

import Prelude

import Control.Error.Util (hush)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Array as Data.Array
import Data.Foldable (foldr)
import Data.Generic (toSpine, class Generic, GenericSpine(SString, SInt, SBoolean, SRecord, SProd), fromSpine, toSignature)
import Data.List (List(Cons, Nil), null)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
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
import Routing.Bob.UrlBoomerang (UrlBoomerang, Url, parseURL, printURL, liftStringPrs, str, int, boolean, liftStringBoomerang)
import Text.Boomerang.Combinators (maph, nil, duck1, cons)
import Text.Boomerang.HStack (type (:-), HNil, hHead, hNil, hSingleton)
import Text.Boomerang.Prim (Boomerang(Boomerang), runSerializer)
import Text.Boomerang.String (lit)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (eof)
import Type.Proxy (Proxy(Proxy))

type UrlBoomerangForGenericSpine r = UrlBoomerang r (GenericSpine :- r)

toSpineBoomerang ::
  forall r.
    SigF (UrlBoomerangForGenericSpine r) -> UrlBoomerangForGenericSpine r
toSpineBoomerang (SigRecordF l) =
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
toSpineBoomerang (SigProdF _ cs@(h :| t)) =
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
      liftStringBoomerang (lit constructorName)
     where
      constructorName = serializeConstructorName constructor.sigConstructor

    bmg
      | null t = constructorBmg
      | null constructor.sigValues = constructorNameBmg <<< constructorBmg
      | otherwise = constructorNameBmg <<< liftStringBoomerang (lit "/") <<< constructorBmg
toSpineBoomerang SigBooleanF =
  maph SBoolean ser <<< boolean
 where
  ser (SBoolean b) = Just b
  ser _            = Nothing
toSpineBoomerang SigIntF =
  maph SInt ser <<< int
 where
  ser (SInt b) = Just b
  ser _        = Nothing
toSpineBoomerang SigStringF =
  maph SString ser <<< str
 where
  ser (SString s) = Just s
  ser _        = Nothing

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
  camelsToHyphens (fromMaybe n (Data.Array.last <<< split (Pattern ".") $ n))

genericToUrl :: forall a. (Generic a) => a -> Maybe String
genericToUrl a = do
  route <- bob (Proxy :: Proxy a)
  url <- serialize route a
  printURL url

genericFromUrl :: forall a. (Generic a) => String -> Maybe a
genericFromUrl s = do
  url <- parseURL s
  route <- bob (Proxy :: Proxy a)
  parse route url

data Router a = Router (UrlBoomerang HNil (a :- HNil))

router :: forall a. (Generic a) => Proxy a -> Maybe (Router a)
router p = do
  b <- bob p
  pure $ Router b

toUrl :: forall a. Router a -> a -> String
toUrl (Router bmg) a =
  unsafePartial (case serialize bmg a >>= printURL of Just s -> s)

fromUrl :: forall a. Router a -> String -> Maybe a
fromUrl (Router bmg) url = do
  p ← parseURL url
  parse bmg p
