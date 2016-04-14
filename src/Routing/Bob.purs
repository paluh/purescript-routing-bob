module Routing.Bob where

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Debug.Trace (trace)
import Data.Array as Array
import Data.Array (length)
import Data.Array.Unsafe (head, tail)
import Data.List as List
import Data.Foldable (foldl, foldr)
import Data.Generic (class Generic, DataConstructor, fromSpine, GenericSignature(..),
                     GenericSpine(..), toSignature, toSpine)
import Data.List (fromFoldable, List)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String (split, toLower)
import Prelude (bind, const, id, return, show, unit, Unit, (<<<), (<>), (==), (<$>), ($))
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

signatureToSpineBoomerang :: forall r. GenericSignature -> Maybe (StringBoomerang r (HCons GenericSpine r))
signatureToSpineBoomerang SigBoolean =
  Just (maph SBoolean ser <<< boolean)
 where
  ser (SBoolean b) = Just b
  ser _            = Nothing
signatureToSpineBoomerang SigInt =
  Just (maph SInt ser <<< int)
 where
  ser (SInt b) = Just b
  ser _        = Nothing
signatureToSpineBoomerang s@(SigProd n cs) = do
  { head : h, tail: t} <- Array.uncons cs
  if length t == 0
    then
      fromConstructor h false
    else do
      hs <- fromConstructor h true
      Array.foldM constructorStep hs t
 where
  constructorStep r c = do
    s <- (fromConstructor c true)
    return (s <> r)

  fromConstructor :: forall s. DataConstructor -> Boolean -> Maybe (StringBoomerang s (HCons GenericSpine s))
  fromConstructor constructor prependWithConstructorName = do
    av <- appendValues
    let bmg = maph (SProd constructor.sigConstructor) ser <<< arrayFromList <<< av <<< nil
    return
      if prependWithConstructorName
        then lit (serializeConstructorName constructor.sigConstructor) <<< bmg
        else bmg
   where

    appendValues = case Array.uncons constructor.sigValues of
      Just { head: h, tail: t } -> do
        h' <- valueStep h
        t' <- Array.foldM (\r e -> do
          e' <- valueStep e
          return (e' </> r)) h' t
        return
          if prependWithConstructorName
            then (lit "/" <<< t')
            else t'
      Nothing -> Just id
     where
      valueStep e =
        (\t -> cons <<< lazy <<< t) <$> signatureToSpineBoomerang (e unit)

    ser (SProd c values) =
      if c == constructor.sigConstructor
        then Just values
        else Nothing
    ser _                = Nothing

    lazy :: forall a t. StringBoomerang (HCons a t) (HCons (Unit -> a) t)
    lazy = maph const (Just <<< (_ $ unit))

  serializeConstructorName n =
    fromMaybe n (Array.last (split "." <<< toLower $ n))
signatureToSpineBoomerang _ = Nothing

bob :: forall a r. (Generic a) => Proxy a -> Maybe (StringBoomerang r (HCons a r))
bob p = do
  sb <- (signatureToSpineBoomerang (toSignature p))
  return (maph prs (\v -> Just (Just v)) <<< maph fromSpine (toSpine <$> _) <<< sb)
 where
  prs (Just s) = s
  prs Nothing = unsafeThrow ("Incorrect spine generated for signature: " <> show (toSignature p))

