module Routing.Bob where

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Debug.Trace (trace)
import Data.Array as Array
import Data.Array.Unsafe (head)
import Data.Foldable (foldl, foldr)
import Data.Generic (class Generic, DataConstructor, fromSpine, GenericSignature(..),
                     GenericSpine(..), toSignature, toSpine)
import Data.List (fromFoldable, List)
import Data.Maybe (Maybe(..))
import Prelude (const, id, show, unit, Unit, (<<<), (<>), (==), (<$>), ($))
import Text.Boomerang.Combinators (cons, maph, nil, pure)
import Text.Boomerang.HStack (HCons, hNil, HNil)
import Text.Boomerang.String (int, lit, noneOf, StringBoomerang, string)
import Type.Proxy (Proxy)

join :: forall a b c. StringBoomerang b c -> StringBoomerang a b -> StringBoomerang a c
join b1 b2 = b1 <<< lit "/" <<< b2

infixl 6 join as </>

boolean :: forall r. StringBoomerang r (HCons Boolean r)
boolean =
  boolean' <<< (string "on" <> string "off")
 where
  boolean' :: forall s. StringBoomerang (HCons String s) (HCons Boolean s)
  boolean' =
    maph prs ser
   where
    prs "on" = true
    prs _    = false

    ser true  = Just "on"
    ser false = Just "off"

arrayFromList :: forall t a tok. Boomerang tok (HCons (List a) t) (HCons (Array a) t)
arrayFromList =
  maph arrayFromFoldable (Just <<< fromFoldable)
 where
  -- very ineficient - will be replaced with next purescript-array release
  arrayFromFoldable = foldr Data.Array.cons []


signatureToSpineBoomerang :: forall r. GenericSignature -> StringBoomerang r (HCons GenericSpine r)
signatureToSpineBoomerang SigBoolean =
  maph SBoolean ser <<< boolean
 where
  ser (SBoolean b) = Just b
  ser _            = Nothing
signatureToSpineBoomerang SigInt =
  maph SInt ser <<< int
 where
  ser (SInt b) = Just b
  ser _        = Nothing
signatureToSpineBoomerang (SigRecord props) =
  toRecord <<< foldl step nil props
 where
  step r e = cons </> (toProp e.recLabel <<< signatureToSpineBoomerang (e.recValue unit)) <<< r

  toProp :: forall s. String -> StringBoomerang (HCons GenericSpine s) (HCons GenericRecProp s)
  toProp l =
    maph propPrs propSer
   where
    propPrs v = {recLabel : l, recValue : \_ -> v}
    propSer p =
      trace ("p.recLabel = " <> p.recLabel <> "; l = " <> l)
        (\_ ->
          if p.recLabel == l
            then Just (p.recValue unit)
            else Nothing)
  toRecord :: forall s. StringBoomerang (HCons (List GenericRecProp) s) (HCons GenericSpine s)
  toRecord =
    maph SRecord recSer <<< arrayFromList
   where
    recSer (SRecord props) = Just props
    recSer _               = Nothing
signatureToSpineBoomerang s@(SigProd n cs) =
  -- XXX: this is coorectly only for single constructor
  let constructor = (head cs) in
  fromConstructor constructor
 where
  fromConstructor :: forall s. DataConstructor -> StringBoomerang s (HCons GenericSpine s)
  fromConstructor constructor =
    maph (SProd constructor.sigConstructor) ser <<< arrayFromList <<< foldl step nil constructor.sigValues
   where
    ser (SProd c values) = Just values
    ser _                = Nothing

    lazy :: forall a t. StringBoomerang (HCons a t) (HCons (Unit -> a) t)
    lazy = maph const (Just <<< (_ $ unit))

    step r e = cons </> lazy <<< signatureToSpineBoomerang (e unit) <<< r

bob :: forall a r. (Generic a) => Proxy a -> StringBoomerang r (HCons a r)
bob p =
  maph prs (\v -> Just (Just v)) <<< maph fromSpine (toSpine <$> _) <<< (signatureToSpineBoomerang (toSignature p))
 where
  prs (Just s) = s
  prs Nothing = unsafeThrow ("Incorrect spine generated for signature: " <> show (toSignature p))

