module Routing.Bob where

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Array as Array
import Data.Array (length)
import Data.Array.Unsafe (head, tail)
import Data.List as List
import Data.List (List(..), (:))
import Data.Foldable (foldl, foldr)
import Data.Generic (class Generic, DataConstructor, fromSpine, GenericSignature(..),
                     GenericSpine(..), toSignature, toSpine)
import Data.List (fromFoldable, List)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String (split, toLower)
import Prelude (bind, const, id, map, return, show, unit, Unit, (<<<), (<>), (==), (<$>), ($), (>))
import Text.Boomerang.Combinators (cons, list, maph, pure, nil)
import Text.Boomerang.HStack (HCons)
import Text.Boomerang.Prim (Boomerang)
import Text.Boomerang.String (int, lit, many1NoneOf, noneOf, parse,
                              StringBoomerang, serialize, string)
import Type.Proxy (Proxy(..))

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

foreign import encodeURIComponent :: String -> String
foreign import decodeURIComponent :: String -> String
foreign import camelsToHyphens :: String -> String

str :: forall r. StringBoomerang r (HCons String r)
str =
  maph (decodeURIComponent) (Just <<< encodeURIComponent) <<< many1NoneOf "/?#"

arrayFromList :: forall t a tok. Boomerang tok (HCons (List a) t) (HCons (Array a) t)
arrayFromList =
  maph arrayFromFoldable (Just <<< fromFoldable)
 where
  -- very ineficient - will be replaced with next purescript-array release
  arrayFromFoldable = foldr Data.Array.cons []

type GenericRecProp = {recLabel :: String, recValue :: Unit -> GenericSpine}

intersperce :: forall tok a t. (List (Boomerang tok (HCons (List a) t) (HCons a (HCons (List a) t)))) ->
                               (forall r. Boomerang tok r r) ->
                               Boomerang tok t (HCons (List a) t)
intersperce Nil _ = nil
intersperce (Cons b t) sep =
  cons <<< b <<< foldr step nil t
 where
  step e l = sep <<< cons <<< e <<< l

signatureToSpineBoomerang :: forall r. GenericSignature -> Maybe (StringBoomerang r (HCons GenericSpine r))
signatureToSpineBoomerang s@(SigProd n cs) = do
  { head : h, tail: t} <- Array.uncons cs
  if length t == 0
    then
      fromConstructor h false
    else do
      hs <- fromConstructor h true
      Array.foldM (\r c -> (_ <> r) <$> (fromConstructor c true)) hs t
 where
  fromConstructor :: DataConstructor -> Boolean -> Maybe (StringBoomerang r (HCons GenericSpine r))
  fromConstructor constructor prependWithConstructorName = do
    let values = map (_ $ unit) constructor.sigValues
    valuesSpines <- Array.foldM (\r e -> (\b -> (lazy <<< b) : r) <$> signatureToSpineBoomerang e) Nil values
    let bmg = maph (SProd constructor.sigConstructor) ser <<< arrayFromList <<< intersperce valuesSpines (lit "/")
    if prependWithConstructorName
      then do
        let cn = serializeConstructorName constructor.sigConstructor
        if length values == 0
          then return (lit cn <<< bmg)
          else return (lit cn </> bmg)
      else return bmg
   where
    lazy :: forall z y. StringBoomerang (HCons y z) (HCons (Unit -> y) z)
    lazy = maph const (Just <<< (_ $ unit))

    ser (SProd c values) =
      if c == constructor.sigConstructor
        then Just values
        else Nothing
    ser _                = Nothing

    serializeConstructorName n =
      camelsToHyphens (fromMaybe n (Array.last <<< split "." $ n))

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
signatureToSpineBoomerang SigString =
  Just (maph SString ser <<< str)
 where
  ser (SString s) = Just s
  ser _        = Nothing
signatureToSpineBoomerang _ = Nothing

bob :: forall a r. (Generic a) => Proxy a -> Maybe (StringBoomerang r (HCons a r))
bob p = do
  sb <- (signatureToSpineBoomerang (toSignature p))
  return (maph prs (\v -> Just (Just v)) <<< maph fromSpine (toSpine <$> _) <<< sb)
 where
  prs (Just s) = s
  prs Nothing = unsafeThrow ("Incorrect spine generated for signature: " <> show (toSignature p))

-- Please use pregenerated route if you care about speed:

toUrl :: forall a. (Generic a) => a -> Maybe String
toUrl a = do
  route <- bob (Proxy :: Proxy a)
  serialize route a

fromUrl :: forall a. (Generic a) => String -> Maybe a
fromUrl s = do
  route <- bob (Proxy :: Proxy a)
  parse route s
