module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(..))
import Prelude (bind, class Eq, class Show, Unit)
import Test.Unit (test, runTest, TIMER)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Assert (equal)
import Text.Boomerang.String (parse, serialize)
import Routing.Bob (bob)
import Type.Proxy (Proxy(..))

data BooleanIntRoute = BooleanIntRoute
  { extended :: Boolean
  , id :: Int
  }
derive instance genericBooleanIntRoute :: Generic BooleanIntRoute

data PrimitivePositionalValue = PrimitivePositionalValue Int
derive instance genericPrimitivePositionalValue :: Generic PrimitivePositionalValue
instance eqPrimitivePositionalValue :: Eq PrimitivePositionalValue where
  eq = gEq
instance showPrimitivePositionalValue :: Show PrimitivePositionalValue where
  show = gShow

data PrimitivePositionalValues = PrimitivePositionalValues Int Boolean Int
derive instance genericPrimitivePositionalValues :: Generic PrimitivePositionalValues
instance eqPrimitivePositionalValues :: Eq PrimitivePositionalValues where
  eq = gEq
instance showPrimitivePositionalValues :: Show PrimitivePositionalValues where
  show = gShow

data UnionOfPrimitivePositionalValues =
    FirstConstructor Int Boolean Int
  | SecondConstructor Int Boolean Int
derive instance genericUnionOfPrimitivePositionalValues :: Generic UnionOfPrimitivePositionalValues
instance eqUnionOfPrimitivePositionalValues :: Eq UnionOfPrimitivePositionalValues where
  eq = gEq
instance showUnionOfPrimitivePositionalValues :: Show UnionOfPrimitivePositionalValues where
  show = gShow

main :: forall e. Eff ( timer :: TIMER
                      , avar :: AVAR
                      , testOutput :: TESTOUTPUT | e
                      ) Unit
main = runTest do
  test "bob handles constructor with single, primitive value" do
    let route = bob (Proxy :: Proxy PrimitivePositionalValue)
        obj = PrimitivePositionalValue 8
    equal (Just "/8") (serialize route obj)
    equal (Just obj) (parse route "/8")

  test "gRoute handles construtor with multiple, primitive values" do
    let route = bob (Proxy :: Proxy PrimitivePositionalValues)
        obj = PrimitivePositionalValues 8 true 9
    equal (Just "/8/on/9") (serialize route obj)
    equal (Just obj) (parse route "/8/on/9")
