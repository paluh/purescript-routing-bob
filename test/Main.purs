module Test.Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Generic (gShow, class Generic, gEq)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Data.StrMap (empty, fromList, StrMap, fromFoldable)
import Data.Tuple (Tuple(Tuple))
import Routing.Bob (Router, serialize, parse, fromUrl, genericFromUrl, genericToUrl, router, toUrl)
import Routing.Bob.UrlBoomerang (Url, param, boolean)
import Test.Unit (suite, failure, test, TIMER)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Text.Boomerang.Combinators (pureBmg)
import Text.Boomerang.HStack (HCons(HCons), hCons, hArg)
import Text.Boomerang.String (int, StringBoomerang, manyNoneOf)
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

any :: forall r. StringBoomerang r (HCons String r)
any = manyNoneOf ""

-- regression tests

data MainWindowRoute = Profile | Inbox | Settings
derive instance genericMainWindowRoute :: Generic MainWindowRoute
data SideBarRoute = Expanded | Minimized
derive instance genericSideBarRoute :: Generic SideBarRoute
data AppRoute = AppRoute MainWindowRoute SideBarRoute
derive instance genericAppRoute :: Generic AppRoute

newtype UrlWrapper = UrlWrapper Url
-- derive instance genericUrlWrapper :: Generic UrlWrapper
instance eqUrlWrapper :: Eq UrlWrapper where
  eq (UrlWrapper url) (UrlWrapper url') = url.path == url'.path && url.query == url'.query
instance showUrlWrapper :: Show UrlWrapper where
  show (UrlWrapper url) = "UrlWrapper { path: " <> show url.path <> ", query: " <> show url.query <> " }"

toUrl' :: forall a. Router a -> a -> UrlWrapper
toUrl' r o = UrlWrapper $ toUrl r o

main :: forall e. Eff ( timer :: TIMER
                      , avar :: AVAR
                      , console :: CONSOLE
                      , testOutput :: TESTOUTPUT | e
                      ) Unit
main = runTest $ suite "Routing.Bob handles" do
  suite "url path" do
    let
      router' :: forall a e'. (Generic a) =>
        Proxy a ->
        (Router a -> Aff (timer :: TIMER , avar :: AVAR , testOutput :: TESTOUTPUT | e') Unit) ->
        Aff (timer :: TIMER , avar :: AVAR , testOutput :: TESTOUTPUT | e') Unit
      router' p t = (case router p of
        Nothing -> failure ("Router generation failed")
        (Just b)-> t b)

    let
      e s = { path: s, query: empty :: StrMap String }
      e' = UrlWrapper <<< e

    test "which contains constructor with single, primitive value" do
      let obj = PrimitivePositionalValue 8
      router' (Proxy :: Proxy PrimitivePositionalValue) (\r -> do
        equal (e' "8") (toUrl' r obj)
        equal (Just obj) (fromUrl r (e "8")))

    test "and consumes whole input" do
      let obj = PrimitivePositionalValue 8
      router' (Proxy :: Proxy PrimitivePositionalValue) (\r -> do
        equal (Nothing) (fromUrl r (e "8/something-more")))

    test "which contains construtor with multiple, primitive values" do
      let obj = PrimitivePositionalValues 8 true 9
      router' (Proxy :: Proxy PrimitivePositionalValues) (\r -> do
        equal (e' "8/on/9") (toUrl' r obj)
        equal (Just obj) (fromUrl r (e "8/on/9")))

    test "which contains multiple empty constructors" do
       let fObj = FirstEmptyConstructor
           sObj = SecondEmptyConstructor
       router' (Proxy :: Proxy UnionOfEmptyConstructors) (\r -> do
         equal (e' "first-empty-constructor") (toUrl' r fObj)
         equal (e' "second-empty-constructor") (toUrl' r sObj)

         equal (Just fObj) (fromUrl r (e "first-empty-constructor"))
         equal (Just sObj) (fromUrl r (e "second-empty-constructor")))

    test "which contains multiple non empty constructors" do
      let fObj = FirstConstructor 8 true 9
          sObj = SecondConstructor false
      router' (Proxy :: Proxy UnionOfPrimitivePositionalValues) (\r -> do
        equal (e' "first-constructor/8/on/9") (toUrl' r fObj)
        equal (e' "second-constructor/off") (toUrl' r sObj)

        equal (Just fObj) (fromUrl r (e "first-constructor/8/on/9"))
        equal (Just sObj) (fromUrl r (e "second-constructor/off")))

    test "through generic helpers" do
        equal
          (Just <<< UrlWrapper <<< e $ "first-constructor/8/on/9")
          (UrlWrapper <$> genericToUrl (FirstConstructor 8 true 9))
        equal
          (Just <<< UrlWrapper <<< e  $ "second-constructor/off")
          (UrlWrapper <$> genericToUrl (SecondConstructor false))

        equal (Just (FirstConstructor 8 true 9)) (genericFromUrl (e "first-constructor/8/on/9"))
        equal (Just (SecondConstructor false)) (genericFromUrl (e "second-constructor/off"))

    test "which contains nested structure with primitive values" do
      let obj = NestedStructureWithPrimitivePositionvalValue (PrimitivePositionalValue 8)
      router' (Proxy :: Proxy NestedStructureWithPrimitivePositionvalValue) (\r -> do
        equal (e' "8") (toUrl' r obj)
        equal (Just obj) (fromUrl r (e "8")))

    test "which contains nesteted structures with mutilple constructors" do
      let fObj = FirstOuterConstructor (FirstConstructor 100 true 888)
          sObj = SecondOuterConstructor (PrimitivePositionalValues 8 false 100)

      router' (Proxy :: Proxy NestedStructures) (\r -> do
        equal (e' "first-outer-constructor/first-constructor/100/on/888") (toUrl' r fObj)
        equal (Just fObj) (fromUrl r (e "first-outer-constructor/first-constructor/100/on/888"))

        equal (e' "second-outer-constructor/8/off/100") (toUrl' r sObj)
        equal (Just sObj) (fromUrl r (e "second-outer-constructor/8/off/100")))

    test "which contains multiple nesteted structures with multiple constructors" do
      router' (Proxy :: Proxy AppRoute) (\r -> do
        equal (e' "profile/minimized") (toUrl' r (AppRoute Profile Minimized)))

    test "and uses correct escaping for string values" do
      let obj = StringValue "this/is?test#string"
      router' (Proxy :: Proxy StringValue) (\r -> do
        equal (e' "this%2Fis%3Ftest%23string") (toUrl' r obj)
        equal (Just obj) (fromUrl r (e "this%2Fis%3Ftest%23string")))
  -- multipleParams :: forall r. UrlBoomerang r (HCons Params r)
  let
    pBmg =
      pureBmg
        (hArg (hArg (hArg hCons)) params)
        (\(HCons (Params r) t) -> Just (hCons r.paramString (hCons r.paramInt (hCons r.paramBoolean t))))
    multipleParams =
      pBmg <<< param "paramString" any <<< param "paramInt" int <<< param "paramBoolean" boolean

  suite "query parsing" do
    test "with single param" do
      let
        query = fromFoldable [Tuple "param" "somevalue"]
        urlBmg = param "param" (manyNoneOf "")
        url = { path: "", query: query }
      equal
        (Just "somevalue")
        (parse urlBmg url)

    test "with multiple parameters" do
      let
        query = fromFoldable [Tuple "paramInt" "8", Tuple "paramBoolean" "off", Tuple "paramString" "somestringvalue"]
        url = { path: "", query: query }
      equal
        (Just $ params "somestringvalue" 8 false)
        (parse multipleParams url)

  suite "query serialization" do
    test "with correct values" do
      equal
        (Just <<< fromList $ (Tuple "paramBoolean" "off") : (Tuple "paramInt" "8") : (Tuple "paramString" "tes") : Nil)
        (_.query <$> (serialize multipleParams (params "tes" 8 false)))

