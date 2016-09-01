module Test.Main where

import Prelude
import Control.Error.Util (hush)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Generic (gShow, class Generic, gEq)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, fromFoldable, empty)
import Data.String (dropWhile)
import Data.Tuple (Tuple(Tuple))
import Data.URI (Query(Query))
import Data.URI.Query (parseQuery)
import Routing.Bob (serialize, parse, Router, Fix(Fix), SigF(SigBooleanF, SigIntF, SigStringF), genericFromUrl, toUrl, genericToUrl, param, fromUrl, router)
import Routing.Bob.UrlBoomerang (UrlBoomerang, liftStringBoomerang, boolean, int)
import Test.Unit (suite, failure, test, TIMER)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Text.Boomerang.Combinators (pureBmg)
import Text.Boomerang.HStack (HCons(HCons), hCons, hArg)
import Text.Boomerang.String (manyNoneOf)
import Text.Parsing.StringParser (runParser)
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

data SumWithInternalRecords
  = FirstConstructorWithRecord
      Int
      { int1 :: Maybe Int
      , string1 :: String
      }
  | SecondConstructorWithRecord
      String
      { int2 :: Maybe Int
      , string2 :: String
      , boolean2 :: Boolean
      }
derive instance genericSumWithInternalRecords :: Generic SumWithInternalRecords

newtype Params =
  Params
    { paramString :: String
    , paramInt :: Int
    , paramBoolean :: Boolean
    }
derive instance genericParms :: Generic Params
derive instance eqParams :: Eq Params
instance showParams :: Show Params where
  show = gShow

params :: String -> Int -> Boolean -> Params
params = ((Params <$> _) <$>  _) <$> {paramString: _, paramInt: _, paramBoolean: _ }

newtype ParamsWithOptionals =
  ParamsWithOptionals
    { optParamString :: Maybe String
    , optParamInt :: Maybe Int
    , optParamBoolean :: Maybe Boolean
    }
derive instance genericParamsWithOptionals :: Generic ParamsWithOptionals
derive instance eqParamsWithOptionals :: Eq ParamsWithOptionals
instance showParamsWithOptionals :: Show ParamsWithOptionals where
  show = gShow

any :: forall r. UrlBoomerang r (HCons String r)
any = liftStringBoomerang $ manyNoneOf ""

-- regression tests

data MainWindowRoute = Profile | Inbox | Settings
derive instance genericMainWindowRoute :: Generic MainWindowRoute
data SideBarRoute = Expanded | Minimized
derive instance genericSideBarRoute :: Generic SideBarRoute
data AppRoute = AppRoute MainWindowRoute SideBarRoute
derive instance genericAppRoute :: Generic AppRoute

main :: forall e. Eff ( timer :: TIMER
                      , avar :: AVAR
                      , console :: CONSOLE
                      , testOutput :: TESTOUTPUT | e
                      ) Unit
main = runTest $ suite "Routing.Bob handles" do
  let
    path s = { path: s, query: empty :: StrMap (Maybe String) }
    query q = { path: "", query: q }
  let
    router' :: forall a e'. (Generic a) =>
      Proxy a ->
      (Router a -> Aff (timer :: TIMER , avar :: AVAR , testOutput :: TESTOUTPUT | e') Unit) ->
      Aff (timer :: TIMER , avar :: AVAR , testOutput :: TESTOUTPUT | e') Unit
    router' p t = (case router p of
      Nothing -> failure ("Router generation failed")
      (Just b)-> t b)

  suite "url path" do
    test "which contains constructor with single, primitive value" do
      let obj = PrimitivePositionalValue 8
      router' (Proxy :: Proxy PrimitivePositionalValue) (\r -> do
        equal "8" (toUrl r obj)
        equal (Just obj) (fromUrl r "8"))
    test "and consumes whole input" do
      let obj = PrimitivePositionalValue 8
      router' (Proxy :: Proxy PrimitivePositionalValue) (\r -> do
        equal (Nothing) (fromUrl r "8/something-more"))

    test "which contains construtor with multiple, primitive values" do
      let obj = PrimitivePositionalValues 8 true 9
      router' (Proxy :: Proxy PrimitivePositionalValues) (\r -> do
        equal "8/on/9" (toUrl r obj)
        equal (Just obj) (fromUrl r "8/on/9"))

    test "which contains multiple empty constructors" do
       let fObj = FirstEmptyConstructor
           sObj = SecondEmptyConstructor
       router' (Proxy :: Proxy UnionOfEmptyConstructors) (\r -> do
         equal "first-empty-constructor" (toUrl r fObj)
         equal "second-empty-constructor" (toUrl r sObj)

         equal (Just fObj) (fromUrl r "first-empty-constructor")
         equal (Just sObj) (fromUrl r "second-empty-constructor"))

    test "which contains multiple non empty constructors" do
      let fObj = FirstConstructor 8 true 9
          sObj = SecondConstructor false
      router' (Proxy :: Proxy UnionOfPrimitivePositionalValues) (\r -> do
        equal "first-constructor/8/on/9" (toUrl r fObj)
        equal "second-constructor/off" (toUrl r sObj)

        equal (Just fObj) (fromUrl r "first-constructor/8/on/9")
        equal (Just sObj) (fromUrl r "second-constructor/off"))

    test "through generic helpers" do
        equal
          (Just "first-constructor/8/on/9")
          (genericToUrl (FirstConstructor 8 true 9))
        equal
          (Just "second-constructor/off")
          (genericToUrl (SecondConstructor false))

        equal (Just (FirstConstructor 8 true 9)) (genericFromUrl "first-constructor/8/on/9")
        equal (Just (SecondConstructor false)) (genericFromUrl "second-constructor/off")

    test "which contains nested structure with primitive values" do
      let obj = NestedStructureWithPrimitivePositionvalValue (PrimitivePositionalValue 8)
      router' (Proxy :: Proxy NestedStructureWithPrimitivePositionvalValue) (\r -> do
        equal "8" (toUrl r obj)
        equal (Just obj) (fromUrl r "8"))

    test "which contains nesteted structures with mutilple constructors" do
      let fObj = FirstOuterConstructor (FirstConstructor 100 true 888)
          sObj = SecondOuterConstructor (PrimitivePositionalValues 8 false 100)

      router' (Proxy :: Proxy NestedStructures) (\r -> do
        equal "first-outer-constructor/first-constructor/100/on/888" (toUrl r fObj)
        equal (Just fObj) (fromUrl r "first-outer-constructor/first-constructor/100/on/888")

        equal "second-outer-constructor/8/off/100" (toUrl r sObj)
        equal (Just sObj) (fromUrl r "second-outer-constructor/8/off/100"))

    test "which contains multiple nesteted structures with multiple constructors" do
      router' (Proxy :: Proxy AppRoute) (\r -> do
        equal "profile/minimized" (toUrl r (AppRoute Profile Minimized)))

    test "and uses correct escaping for string values" do
      let obj = StringValue "this/is?test#string"
      router' (Proxy :: Proxy StringValue) (\r -> do
        equal "this%2Fis%3Ftest%23string" (toUrl r obj)
        equal (Just obj) (fromUrl r "this%2Fis%3Ftest%23string"))
  -- multipleParams :: forall r. UrlBoomerang r (HCons Params r)
  suite "Data.Maybe in different way" do
    test "and Just constrcutor is not used" do
      equal
        (Just "8")
        (genericToUrl (Just 8))
      equal
        (Just (Just 8))
        (genericFromUrl "8")
    test "and Nothing is just absent" do
      let nothing = Nothing :: Maybe Int
      equal
        (Just "")
        (genericToUrl nothing)
      equal
        (Just nothing)
        (genericFromUrl "")
  let
    pBmg =
      pureBmg
        (hArg (hArg (hArg hCons)) params)
        (\(HCons (Params r) t) -> Just (hCons r.paramString (hCons r.paramInt (hCons r.paramBoolean t))))
    multipleParams =
      pBmg <<< param "paramString" { a: any, f: Fix SigStringF } <<< param "paramInt" { a: int, f: Fix SigIntF } <<< param "paramBoolean" { a: boolean, f: Fix SigBooleanF }

  suite "query parsing" do
    test "with single param" do
      let
        q = fromFoldable [Tuple "param" (Just "somevalue")]
        urlBmg = param "param" { a: (liftStringBoomerang $ manyNoneOf ""), f: Fix SigStringF }
        url = query q
      equal
        (Just "somevalue")
        (parse urlBmg url)

    test "with multiple parameters" do
      let
        q =
          fromFoldable
            [ Tuple "paramInt" (Just "8")
            , Tuple "paramBoolean" (Just "off")
            , Tuple "paramString" (Just "somestringvalue")
            ]
        url = query q
      equal
        (Just $ params "somestringvalue" 8 false)
        (parse multipleParams url)

  suite "query serialization" do
    test "with correct values" do
      equal
        (Just <<< fromFoldable $ [ Tuple "paramBoolean" (Just "off"), Tuple "paramInt" (Just "8"), Tuple "paramString" (Just "tes")])
        (_.query <$> (serialize multipleParams (params "tes" 8 false)))

  suite "query with optional values" do
    let
      q =
        fromFoldable
          [ Tuple "optParamString" Nothing
          , Tuple "optParamBoolean" (Just "off")
          ]
      p = ParamsWithOptionals { optParamBoolean: Just false, optParamInt: Nothing, optParamString: Nothing }
    suite "parsing" do
      test "through generic helper from String" do
        equal
          (Just p)
          (genericFromUrl "?optParamString=&optParamBoolean=off")
    suite "serialization" do
      test "through generic helper from String" do
        equal
          (Just "?optParamBoolean=off")
          (genericToUrl p)

  suite "query" do
    let
      q =
        fromFoldable
          [ Tuple "paramString" (Just "test")
          , Tuple "paramInt" (Just "8")
          , Tuple "paramBoolean" (Just "off")
          ]
      p = Params { paramBoolean: false, paramInt: 8, paramString: "test" }
    suite "serialization" do
      test "through generic helper" do
        equal
          (Just <<< Query $ q)
          (genericToUrl p >>= (\q' -> hush <<< runParser parseQuery <<< dropWhile ('?' == _) $ q'))
      test "for constructor with records" do
        let fObj = FirstConstructorWithRecord 8 { int1: Just 8, string1: "string1value" }
            sObj = SecondConstructorWithRecord "test" { int2: Nothing, string2: "string2value", boolean2: false }

        router' (Proxy :: Proxy SumWithInternalRecords) (\r -> do
          equal "first-constructor-with-record/8/?string1=string1value&int1=8" (toUrl r fObj)
          equal "second-constructor-with-record/test/?string2=string2value&boolean2=off" (toUrl r sObj))

    suite "parsing" do
      test "through generic helper from String" do
        equal
          (Just p)
          (genericFromUrl "?paramString=test&paramInt=8&paramBoolean=off")
