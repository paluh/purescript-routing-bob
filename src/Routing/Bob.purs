module Routing.Bob where

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Debug.Trace (trace)
import Data.Array as Array
import Data.Array (length)
import Data.Array.Unsafe (head, tail)
import Data.Foldable (foldl, foldr)
import Data.Generic (class Generic, DataConstructor, fromSpine, GenericSignature(..),
                     GenericSpine(..), toSignature, toSpine)
import Data.List (fromFoldable, List)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String (split, toLower)
import Prelude (const, id, show, unit, Unit, (<<<), (<>), (==), (<$>), ($))
import Text.Boomerang.Combinators (cons, maph, nil)
import Text.Boomerang.HStack (HCons)
import Text.Boomerang.Prim (Boomerang)
import Text.Boomerang.String (int, lit, StringBoomerang, string)
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

type GenericRecProp = {recLabel :: String, recValue :: Unit -> GenericSpine}

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
  -- XXX: rewrite this ugly piece with proper failure
  let constructor = (head cs)
  in
    if length cs == 1
      then fromConstructor constructor false
    else
      foldl constructorStep (fromConstructor constructor true) (tail cs)
 where
  constructorStep r c = (fromConstructor c true) <> r

  fromConstructor :: forall s. DataConstructor -> Boolean -> StringBoomerang s (HCons GenericSpine s)
  fromConstructor constructor prependConstructorName =
    if prependConstructorName
      then lit (serializeConstructorName constructor.sigConstructor) <<< bmg
      else bmg
   where
    bmg = maph (SProd constructor.sigConstructor) ser <<< arrayFromList <<< appendValues <<< nil

    appendValues = case Array.uncons constructor.sigValues of
      Just { head: h, tail: t } ->
        (if prependConstructorName then lit "/" else id)
        <<< foldl (\r e -> valueStep e </> r) (valueStep h) t
      Nothing -> id
     where
      valueStep e = cons <<< lazy <<< signatureToSpineBoomerang (e unit)

    ser (SProd c values) =
      if c == constructor.sigConstructor
        then Just values
        else Nothing
    ser _                = Nothing

    lazy :: forall a t. StringBoomerang (HCons a t) (HCons (Unit -> a) t)
    lazy = maph const (Just <<< (_ $ unit))

  serializeConstructorName n =
    fromMaybe n (Array.last (split "." <<< toLower $ n))


bob :: forall a r. (Generic a) => Proxy a -> StringBoomerang r (HCons a r)
bob p =
  maph prs (\v -> Just (Just v)) <<< maph fromSpine (toSpine <$> _) <<< (signatureToSpineBoomerang (toSignature p))
 where
  prs (Just s) = s
  prs Nothing = unsafeThrow ("Incorrect spine generated for signature: " <> show (toSignature p))

