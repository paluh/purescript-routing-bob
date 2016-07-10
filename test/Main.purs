module Test.Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(..))
import Routing.Bob (bob, fromUrl, toUrl)
import Test.Unit (suite, failure, test, TIMER)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Text.Boomerang.HStack (HCons)
import Text.Boomerang.String (parse, serialize, StringBoomerang)
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

data UnionOfEmptyConstructors = FirstEmptyConstructor | SecondEmptyConstructor
derive instance genericUnionOfEmptyConstructors :: Generic UnionOfEmptyConstructors
instance eqUnionOfEmptyConstructors :: Eq UnionOfEmptyConstructors where
  eq = gEq
instance showUnionOfEmptyConstructors :: Show UnionOfEmptyConstructors where
  show = gShow

data UnionOfPrimitivePositionalValues =
    FirstConstructor Int Boolean Int
  | SecondConstructor Boolean
derive instance genericUnionOfPrimitivePositionalValues :: Generic UnionOfPrimitivePositionalValues
instance eqUnionOfPrimitivePositionalValues :: Eq UnionOfPrimitivePositionalValues where
  eq = gEq
instance showUnionOfPrimitivePositionalValues :: Show UnionOfPrimitivePositionalValues where
  show = gShow

data NestedStructureWithPrimitivePositionvalValue =
  NestedStructureWithPrimitivePositionvalValue PrimitivePositionalValue
derive instance genericNestedStructureWithPrimitivePositionvalValue :: Generic NestedStructureWithPrimitivePositionvalValue
instance eqNestedStructureWithPrimitivePositionvalValue :: Eq NestedStructureWithPrimitivePositionvalValue where
  eq = gEq
instance showNestedStructureWithPrimitivePositionvalValue :: Show NestedStructureWithPrimitivePositionvalValue where
  show = gShow

data NestedStructures =
    FirstOuterConstructor UnionOfPrimitivePositionalValues
  | SecondOuterConstructor PrimitivePositionalValues
derive instance genericNestedStructures :: Generic NestedStructures
instance eqNestedStructures :: Eq NestedStructures where
  eq = gEq
instance showNestedStructures:: Show NestedStructures where
  show = gShow

data StringValue = StringValue String
derive instance genericStringValue :: Generic StringValue
instance eqStringValue :: Eq StringValue where
  eq = gEq
instance showStringValue:: Show StringValue where
  show = gShow


main :: forall e. Eff ( timer :: TIMER
                      , avar :: AVAR
                      , console :: CONSOLE
                      , testOutput :: TESTOUTPUT | e
                      ) Unit
main = runTest $ suite "Test" do
  let
    bob' :: forall t a e'. (Generic a) =>
      Proxy a ->
      (StringBoomerang t (HCons a t) -> Aff (timer :: TIMER , avar :: AVAR , testOutput :: TESTOUTPUT | e') Unit) ->
      Aff (timer :: TIMER , avar :: AVAR , testOutput :: TESTOUTPUT | e') Unit
    bob' p t = (case bob p of
      Nothing -> failure ("Bob generation failed")
      (Just b)-> t b)
  test "bob handles constructor with single, primitive value" do
    let obj = PrimitivePositionalValue 8
    bob' (Proxy :: Proxy PrimitivePositionalValue) (\route -> do
      equal (Just "8") (serialize route obj)
      equal (Just obj) (parse route "8"))

  test "bob parses whole input" do
    let obj = PrimitivePositionalValue 8
    bob' (Proxy :: Proxy PrimitivePositionalValue) (\route -> do
      equal (Nothing) (parse route "8/something-more"))

  test "bob handles construtor with multiple, primitive values" do
    let obj = PrimitivePositionalValues 8 true 9
    bob' (Proxy :: Proxy PrimitivePositionalValues) (\route -> do
      equal (Just "8/on/9") (serialize route obj)
      equal (Just obj) (parse route "8/on/9"))

  test "bob handles multiple empty constructors" do
    let fObj = FirstEmptyConstructor
        sObj = SecondEmptyConstructor
    bob' (Proxy :: Proxy UnionOfEmptyConstructors) (\route -> do
      equal (Just "first-empty-constructor") (serialize route fObj)
      equal (Just fObj) (parse route "first-empty-constructor")
      equal (Just sObj) (parse route "second-empty-constructor")

      equal (Just "second-empty-constructor") (serialize route sObj))

  test "bob handles multiple non empty constructors" do
    let fObj = FirstConstructor 8 true 9
        sObj = SecondConstructor false
    bob' (Proxy :: Proxy UnionOfPrimitivePositionalValues) (\route -> do
      equal (Just "first-constructor/8/on/9") (serialize route fObj)
      equal (Just "second-constructor/off") (serialize route sObj)

      equal (Just fObj) (parse route "first-constructor/8/on/9")
      equal (Just sObj) (parse route "second-constructor/off"))

  test "we can avoid bob and use direct functions" do
      equal (Just "first-constructor/8/on/9") (toUrl (FirstConstructor 8 true 9))
      equal (Just "second-constructor/off") (toUrl (SecondConstructor false))

      equal (Just (FirstConstructor 8 true 9)) (fromUrl "first-constructor/8/on/9")
      equal (Just (SecondConstructor false)) (fromUrl "second-constructor/off")


  test "bob handles nested structure with primitive value" do
    let obj = NestedStructureWithPrimitivePositionvalValue (PrimitivePositionalValue 8)
    bob' (Proxy :: Proxy NestedStructureWithPrimitivePositionvalValue) (\route -> do
      equal (Just "8") (serialize route obj)
      equal (Just obj) (parse route "8"))

  test "bob handles nesteted structures with mutilple constructors" do
    let fObj = FirstOuterConstructor (FirstConstructor 100 true 888)
        sObj = SecondOuterConstructor (PrimitivePositionalValues 8 false 100)

    bob' (Proxy :: Proxy NestedStructures) (\route -> do
      equal (Just "first-outer-constructor/first-constructor/100/on/888") (serialize route fObj)
      equal (Just fObj) (parse route "first-outer-constructor/first-constructor/100/on/888")

      equal (Just "second-outer-constructor/8/off/100") (serialize route sObj)
      equal (Just sObj) (parse route "second-outer-constructor/8/off/100"))

  test "bob uses correct escaping for string values" do
    let obj = StringValue "this/is?test#string"
    bob' (Proxy :: Proxy StringValue) (\route -> do
      equal (Just "this%2Fis%3Ftest%23string") (serialize route obj)
      equal (Just obj) (parse route "this%2Fis%3Ftest%23string"))
