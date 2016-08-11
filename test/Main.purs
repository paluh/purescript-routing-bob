module Test.Main where

import Prelude
import Control.Error.Util (hush)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (Either(Left))
import Data.Generic (gShow, class Generic, gEq)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Data.StrMap (fromList, StrMap, empty, fromFoldable)
import Data.Tuple (Tuple(Tuple))
import Routing.Bob (boolean, UrlBoomerang, param, UrlBoomerangToken, Router, fromUrl, toUrl, genericFromUrl, genericToUrl, router)
import Test.Unit (suite, failure, test, TIMER)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Text.Boomerang.Combinators (pureBmg)
import Text.Boomerang.HStack (hSingleton, hCons, hArg, hNil, hHead, HCons(HCons), HNil(HNil))
import Text.Boomerang.Prim (runSerializer, Boomerang(Boomerang))
import Text.Boomerang.String (int, StringBoomerang, manyNoneOf)
import Text.Parsing.Parser (ParseError(ParseError), runParser)
import Text.Parsing.Parser.Pos (initialPos)
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

parse :: forall a. UrlBoomerang HNil (HCons a HNil) -> UrlBoomerangToken -> Maybe a
parse (Boomerang b) s = do
  f <- hush (runParser s (do
    r <- b.prs
    -- XXX: check whether query string was completely consumed
    -- eof
    pure r))
  pure (hHead (f hNil))

serialize :: forall a. UrlBoomerang HNil (HCons a HNil) -> a -> Maybe UrlBoomerangToken
serialize (Boomerang b) s = do
  (Tuple f _) <- runSerializer b.ser (hSingleton s)
  pure (f ({ path: "", query: empty }))

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
main = runTest $ suite "Test" do
  let
    router' :: forall a e'. (Generic a) =>
      Proxy a ->
      (Router a -> Aff (timer :: TIMER , avar :: AVAR , testOutput :: TESTOUTPUT | e') Unit) ->
      Aff (timer :: TIMER , avar :: AVAR , testOutput :: TESTOUTPUT | e') Unit
    router' p t = (case router p of
      Nothing -> failure ("Router generation failed")
      (Just b)-> t b)

  test "router handles constructor with single, primitive value" do
    let obj = PrimitivePositionalValue 8
    router' (Proxy :: Proxy PrimitivePositionalValue) (\r -> do
      equal ("8") (toUrl r obj)
      equal (Just obj) (fromUrl r "8"))

  test "router parses whole input" do
    let obj = PrimitivePositionalValue 8
    router' (Proxy :: Proxy PrimitivePositionalValue) (\r -> do
      equal (Nothing) (fromUrl r "8/something-more"))

  test "router handles construtor with multiple, primitive values" do
    let obj = PrimitivePositionalValues 8 true 9
    router' (Proxy :: Proxy PrimitivePositionalValues) (\r -> do
      equal ("8/on/9") (toUrl r obj)
      equal (Just obj) (fromUrl r "8/on/9"))

  test "bob handles multiple empty constructors" do
    let fObj = FirstEmptyConstructor
        sObj = SecondEmptyConstructor
    router' (Proxy :: Proxy UnionOfEmptyConstructors) (\r -> do
      equal ("first-empty-constructor") (toUrl r fObj)
      equal ("second-empty-constructor") (toUrl r sObj)

      equal (Just fObj) (fromUrl r "first-empty-constructor")
      equal (Just sObj) (fromUrl r "second-empty-constructor"))

  test "bob handles multiple non empty constructors" do
    let fObj = FirstConstructor 8 true 9
        sObj = SecondConstructor false
    router' (Proxy :: Proxy UnionOfPrimitivePositionalValues) (\r -> do
      equal ("first-constructor/8/on/9") (toUrl r fObj)
      equal ("second-constructor/off") (toUrl r sObj)

      equal (Just fObj) (fromUrl r "first-constructor/8/on/9")
      equal (Just sObj) (fromUrl r "second-constructor/off"))

  test "we can avoid router and use direct functions" do
      equal (Just "first-constructor/8/on/9") (genericToUrl (FirstConstructor 8 true 9))
      equal (Just "second-constructor/off") (genericToUrl (SecondConstructor false))

      equal (Just (FirstConstructor 8 true 9)) (genericFromUrl "first-constructor/8/on/9")
      equal (Just (SecondConstructor false)) (genericFromUrl "second-constructor/off")

  test "router handles nested structure with primitive value" do
    let obj = NestedStructureWithPrimitivePositionvalValue (PrimitivePositionalValue 8)
    router' (Proxy :: Proxy NestedStructureWithPrimitivePositionvalValue) (\r -> do
      equal ("8") (toUrl r obj)
      equal (Just obj) (fromUrl r "8"))

  test "router handles nesteted structures with mutilple constructors" do
    let fObj = FirstOuterConstructor (FirstConstructor 100 true 888)
        sObj = SecondOuterConstructor (PrimitivePositionalValues 8 false 100)

    router' (Proxy :: Proxy NestedStructures) (\r -> do
      equal ("first-outer-constructor/first-constructor/100/on/888") (toUrl r fObj)
      equal (Just fObj) (fromUrl r "first-outer-constructor/first-constructor/100/on/888")

      equal ("second-outer-constructor/8/off/100") (toUrl r sObj)
      equal (Just sObj) (fromUrl r "second-outer-constructor/8/off/100"))

  test "router handles multiple nesteted structures with multiple constructors" do
    router' (Proxy :: Proxy AppRoute) (\r -> do
      equal ("profile/minimized") (toUrl r (AppRoute Profile Minimized)))

  test "router uses correct escaping for string values" do
    let obj = StringValue "this/is?test#string"
    router' (Proxy :: Proxy StringValue) (\r -> do
      equal ("this%2Fis%3Ftest%23string") (toUrl r obj)
      equal (Just obj) (fromUrl r "this%2Fis%3Ftest%23string"))
  -- multipleParams :: forall r. UrlBoomerang r (HCons Params r)
  let
    pBmg =
      pureBmg
        (hArg (hArg (hArg hCons)) params)
        (\(HCons (Params r) t) -> Just (hCons r.paramString (hCons r.paramInt (hCons r.paramBoolean t))))
    multipleParams =
      pBmg <<< param "paramString" any <<< param "paramInt" int <<< param "paramBoolean" boolean

  suite "Query parsing" do
    test "single param parsing" do
      let
        query = fromFoldable [Tuple "param" "somevalue"]
        urlBmg = param "param" (manyNoneOf "")
        url = { path: "/", query: query }
      equal
        (Just "somevalue")
        (parse urlBmg url)

    test "multiple parameters parsing" do
      let
        query = fromFoldable [Tuple "paramInt" "8", Tuple "paramBoolean" "off", Tuple "paramString" "somestringvalue"]
        url = { path: "/", query: query }
      equal
        (parse multipleParams url)
        (Just $ params "somestringvalue" 8 false)

  suite "Query serialization" do
    test "serialization" do
      equal
        (Just <<< fromList $ (Tuple "paramBoolean" "off") : (Tuple "paramInt" "8") : (Tuple "paramString" "tes") : Nil)
        (_.query <$> (serialize multipleParams (params "tes" 8 false)))

