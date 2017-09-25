module Test.Main where

import Prelude

import Control.Error.Util (hush)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import Data.Generic (gShow, class Generic, gEq)
import Data.Generic.Rep as Generic.Rep
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..), isJust)
import Data.StrMap (StrMap, empty)
import Data.String (Pattern(..), dropWhile, lastIndexOf)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(Tuple))
import Data.URI (Query(Query))
import Data.URI.Query as Query
import Routing.Bob (Router(..), defaultOptions, fromUrl, genericFromUrl', genericToUrl', router, router', toUrl)
import Routing.Bob.UrlBoomerang (UrlBoomerang, liftStringBoomerang)
import Test.Unit (suite, failure, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Text.Boomerang.Generic (constructorBoomerang)
import Text.Boomerang.HStack (HCons, type (:-))
import Text.Boomerang.String (lit, manyNoneOf)
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
derive instance genericRepUnionOfPrimitivePositionalValues ∷ Generic.Rep.Generic UnionOfPrimitivePositionalValues _

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

newtype ParamsWithMaybes =
  ParamsWithMaybes
    { optParamString :: Maybe String
    , optParamInt :: Maybe Int
    , optParamBoolean :: Maybe Boolean
    }
derive instance genericParamsWithMaybes :: Generic ParamsWithMaybes
derive instance eqParamsWithMaybes :: Eq ParamsWithMaybes
instance showParamsWithMaybes :: Show ParamsWithMaybes where
  show = gShow

data Optional a = Value a | Missing
derive instance genericOptional :: Generic a => Generic (Optional a)
derive instance eqOptional :: Eq a => Eq (Optional a)
instance showOptional :: Generic a => Show (Optional a) where
  show = gShow

newtype ParamsWithOptionals =
  ParamsWithOptionals
    { optParamString :: Optional String
    , optParamInt :: Optional Int
    , optParamBoolean :: Optional Boolean
    }
derive instance genericParamsWithOptionals :: Generic ParamsWithOptionals
derive instance eqParamsWithOptionals :: Eq ParamsWithOptionals
instance showParamsWithOptionals :: Show ParamsWithOptionals where
  show = gShow

newtype RecordWithArrays =
  RecordWithArrays
    { arrayOfStrings :: Array String
    , arrayOfInts :: Array Int
    , arrayOfBooleans :: Array Boolean
    }
derive instance genericRecordWithArrays :: Generic RecordWithArrays
derive instance eqRecordWithArrays :: Eq RecordWithArrays
instance showRecordWithArrays :: Show RecordWithArrays where
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

-- Urls with integrated root path
data UrlsWithRoot = Root | Page1 String | Page2 Int
derive instance genericUrlsWithRoot ∷ Generic UrlsWithRoot
instance eqUrlsWithRoot :: Eq UrlsWithRoot where
  eq = gEq
instance showUrlsWithRoot :: Show UrlsWithRoot where
  show = gShow

-- Another strategy to handle root path
data WithRoot r = WRoot | WSub r
derive instance genericRepWithRoot ∷ Generic.Rep.Generic (WithRoot a) _
derive instance genericWithRoot ∷ (Generic a) ⇒ Generic (WithRoot a)
instance eqWithRoot :: (Generic (WithRoot a)) ⇒ Eq (WithRoot a) where
  eq = gEq
instance showWithRoot :: (Generic (WithRoot a)) ⇒ Show (WithRoot a) where
  show = gShow

subrouteR :: forall a r
  . UrlBoomerang r (a :- r)
  → UrlBoomerang r (WithRoot a :- r)
subrouteR r = constructorBoomerang (SProxy :: SProxy "WSub") <<< r
rootR :: forall a r. UrlBoomerang r (WithRoot a :- r)
rootR = constructorBoomerang (SProxy :: SProxy "WRoot") <<< liftStringBoomerang (lit "")

addRoot :: forall a t
  . (Generic.Rep.Generic a t)
  ⇒ Router a
  -> Router (WithRoot a)
addRoot (Router r) =
  Router (subrouteR r <> rootR)

isSuffixOf :: Pattern -> String -> Boolean
isSuffixOf suffix = isJust <<< lastIndexOf suffix

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
    withRouter :: forall a e'. (Generic a) =>
      Proxy a ->
      (Router a -> Aff (timer :: TIMER , avar :: AVAR , testOutput :: TESTOUTPUT | e') Unit) ->
      Aff (timer :: TIMER , avar :: AVAR , testOutput :: TESTOUTPUT | e') Unit
    withRouter p t = (case router' p of
      Nothing -> failure ("Router generation failed")
      (Just b)-> t b)

  suite "url path" do
    test "which contains constructor with single, primitive value" do
      let obj = PrimitivePositionalValue 8
      withRouter (Proxy :: Proxy PrimitivePositionalValue) (\r -> do
        equal "8" (toUrl r obj)
        equal (Just obj) (fromUrl r "8")
        equal (Just obj) (fromUrl r "8?"))
    test "and consumes whole input" do
      let obj = PrimitivePositionalValue 8
      withRouter (Proxy :: Proxy PrimitivePositionalValue) (\r -> do
        equal (Nothing) (fromUrl r "8/something-more"))

    test "which contains construtor with multiple, primitive values" do
      let obj = PrimitivePositionalValues 8 true 9
      withRouter (Proxy :: Proxy PrimitivePositionalValues) (\r -> do
        equal "8/on/9" (toUrl r obj)
        equal (Just obj) (fromUrl r "8/on/9"))

    test "which contains multiple empty constructors" do
       let fObj = FirstEmptyConstructor
           sObj = SecondEmptyConstructor
       withRouter (Proxy :: Proxy UnionOfEmptyConstructors) (\r -> do
         equal "first-empty-constructor" (toUrl r fObj)
         equal "second-empty-constructor" (toUrl r sObj)

         equal (Just fObj) (fromUrl r "first-empty-constructor")
         equal (Just sObj) (fromUrl r "second-empty-constructor"))

    test "which contains multiple non empty constructors" do
      let fObj = FirstConstructor 8 true 9
          sObj = SecondConstructor false
      withRouter (Proxy :: Proxy UnionOfPrimitivePositionalValues) (\r -> do
        equal "first-constructor/8/on/9" (toUrl r fObj)
        equal "second-constructor/off" (toUrl r sObj)

        equal (Just fObj) (fromUrl r "first-constructor/8/on/9")
        equal (Just sObj) (fromUrl r "second-constructor/off"))

    test "root path encoding" do
      let rObj = Root
          serializeConstructorName s | (Pattern ".Root") `isSuffixOf` s = ""
          serializeConstructorName s = defaultOptions.serializeConstructorName s
      case router ({ serializeConstructorName: serializeConstructorName }) (Proxy :: Proxy UrlsWithRoot) of
        Nothing -> failure ("Router generation failed")
        Just r -> do
          equal "" (toUrl r Root)
          equal (Just Root) (fromUrl r "")

          equal "page1/param" (toUrl r (Page1 "param"))
          equal "page2/8" (toUrl r (Page2 8))
          equal (Just (Page1 "param")) (fromUrl r "page1/param")

    test "added root path encoding" do
      let
        fObj = WSub (FirstConstructor 8 true 9)
        sObj = WSub (SecondConstructor false)
        rObj = WRoot
      case router defaultOptions (Proxy :: Proxy UnionOfPrimitivePositionalValues) of
        Nothing -> failure ("Router generation failed")
        Just r -> do
          let r' = addRoot r
          equal "" (toUrl r' WRoot)
          equal (Just WRoot) (fromUrl r' "")

          equal "first-constructor/8/on/3" (toUrl r' (WSub (FirstConstructor 8 true 3)))
          equal "second-constructor/off" (toUrl r' (WSub (SecondConstructor false)))

    test "through generic helpers" do
        equal
          (Just "first-constructor/8/on/9")
          (genericToUrl' (FirstConstructor 8 true 9))
        equal
          (Just "second-constructor/off")
          (genericToUrl' (SecondConstructor false))

        equal (Just (FirstConstructor 8 true 9)) (genericFromUrl' "first-constructor/8/on/9")
        equal (Just (SecondConstructor false)) (genericFromUrl' "second-constructor/off")

    test "through generic helpers mixed with boomerang generic" do
        equal
          (Just "first-constructor/8/on/9")
          (genericToUrl' (FirstConstructor 8 true 9))
        equal
          (Just "second-constructor/off")
          (genericToUrl' (SecondConstructor false))

        equal (Just (FirstConstructor 8 true 9)) (genericFromUrl' "first-constructor/8/on/9")
        equal (Just (SecondConstructor false)) (genericFromUrl' "second-constructor/off")

    test "which contains nested structure with primitive values" do
      let obj = NestedStructureWithPrimitivePositionvalValue (PrimitivePositionalValue 8)
      withRouter (Proxy :: Proxy NestedStructureWithPrimitivePositionvalValue) (\r -> do
        equal "8" (toUrl r obj)
        equal (Just obj) (fromUrl r "8"))

    test "which contains nesteted structures with mutilple constructors" do
      let fObj = FirstOuterConstructor (FirstConstructor 100 true 888)
          sObj = SecondOuterConstructor (PrimitivePositionalValues 8 false 100)

      withRouter (Proxy :: Proxy NestedStructures) (\r -> do
        equal "first-outer-constructor/first-constructor/100/on/888" (toUrl r fObj)
        equal (Just fObj) (fromUrl r "first-outer-constructor/first-constructor/100/on/888")

        equal "second-outer-constructor/8/off/100" (toUrl r sObj)
        equal (Just sObj) (fromUrl r "second-outer-constructor/8/off/100"))

    test "which contains multiple nesteted structures with multiple constructors" do
      withRouter (Proxy :: Proxy AppRoute) (\r -> do
        equal "profile/minimized" (toUrl r (AppRoute Profile Minimized)))

    test "and uses correct escaping for string values" do
      let obj = StringValue "this is test%string"
      withRouter (Proxy :: Proxy StringValue) (\r -> do
        equal "this%20is%20test%25string" (toUrl r obj)
        equal (Just obj) (fromUrl r "this%20is%20test%25string"))
  suite "query with optional values" do
    let
      p = ParamsWithMaybes { optParamBoolean: Just false, optParamInt: Nothing, optParamString: Nothing }
    suite "parsing" do
      test "through generic helper from String" do
        equal
          (Just p)
          (genericFromUrl' "?optParamString&optParamBoolean=off")
    suite "serialization" do
      test "through generic helper from String" do
        equal
          (Just "?optParamBoolean=off")
          (genericToUrl' p)

  suite "query" do
    let
      q =  Tuple "paramString" (Just "test") :  Tuple "paramInt" (Just "8") : Tuple "paramBoolean" (Just "off") : Nil
      p = Params { paramBoolean: false, paramInt: 8, paramString: "test" }
    suite "serialization" do
      test "through generic helper" do
        equal
          (Just <<< Query $ q)
          (genericToUrl' p >>= (\q' -> hush <<< runParser Query.parser <<< dropWhile ('?' == _) $ q'))
      test "for constructor with records and maybe values" do
        let fObj = FirstConstructorWithRecord 8 { int1: Just 8, string1: "string1value" }
            sObj = SecondConstructorWithRecord "test" { int2: Nothing, string2: "string2value", boolean2: false }

        withRouter (Proxy :: Proxy SumWithInternalRecords) (\r -> do
          equal "first-constructor-with-record/8/?string1=string1value&int1=8" (toUrl r fObj)
          equal "second-constructor-with-record/test/?string2=string2value&boolean2=off" (toUrl r sObj))
      test "for constructor with records and values isomorphic to maybe" do
        let fObj = ParamsWithOptionals { optParamString: Value "test", optParamInt: Missing, optParamBoolean: Missing}
            sObj = ParamsWithOptionals { optParamString: Missing, optParamInt: Value 8, optParamBoolean: Missing}

        withRouter (Proxy :: Proxy ParamsWithOptionals) (\r -> do
          equal "?optParamString=test" (toUrl r fObj)
          equal (Just sObj) (fromUrl r "?optParamInt=8&optParamBoolean"))
    suite "parsing" do
      test "through generic helper from String" do
        equal
          (Just p)
          (genericFromUrl' "?paramString=test&paramInt=8&paramBoolean=off")

  suite "query with arrays" do
    let
      p = RecordWithArrays { arrayOfStrings: ["string1", "string2"], arrayOfInts: [1], arrayOfBooleans: [] }
    suite "parsing" do
      test "through generic helper from String" do
        equal
          (Just p)
          (genericFromUrl' "?arrayOfInts=1&arrayOfStrings=string1&arrayOfStrings=string2")
    suite "serialization" do
      test "through generic helper to String" do
        equal
          (Just "?arrayOfStrings=string1&arrayOfStrings=string2&arrayOfInts=1")
          (genericToUrl' p)
    test "with root prefix" do
      let obj = PrimitivePositionalValues 8 true 9
      withRouter (Proxy :: Proxy PrimitivePositionalValues) (\(Router r) -> do
        let r' = Router (liftStringBoomerang (lit "/") <<< r)
        equal "/8/on/9" (toUrl r' obj)
        equal (Just obj) (fromUrl r' "/8/on/9"))
