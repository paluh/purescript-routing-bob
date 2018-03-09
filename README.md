# purescript-routing-bob

Let me introduce to you Bob The Router (known also as "Pendulum Bob") - simple (but usable) example of partial bidirectional routes generator based on [purescript-boomerang](https://github.com/paluh/purescript-boomerang) and [purescript-generics](https://github.com/purescript/purescript-generics).

## Warning

This library is based on old generic interface and needs a rewrite in order to work with [purescript-generic-reps](https://github.com/purescript/purescript-generic-reps). I'm not sure if it preserves current API after this upgrade...

## Installation

```shell
bower install purescript-routing-bob
```

## Usage

Just to give you a hint what this library does, let's copy some tests' fragments (sorry for long data types and constructor names):

  * simple, union type:

    ```purescript
    import Data.Generic (class Generic)
    import Routing.Bob (bob, toUrl, fromUrl)
    import Type.Proxy (Proxy(..))


    data UnionOfPrimitivePositionalValues =
        FirstConstructor Int Boolean Int
      | SecondConstructor Boolean
    derive instance genericUnionOfPrimitivePositionalValues :: Generic UnionOfPrimitivePositionalValues


    -- and related tests' lines

    equal (Just "first-constructor/8/on/9") (genericToUrl (FirstConstructor 8 true 9))
    equal (Just "second-constructor/off") (genericToUrl (SecondConstructor false))

    equal (Just (FirstConstructor 8 true 9)) (genericFromUrl "first-constructor/8/on/9")
    equal (Just (SecondConstructor false)) (genericFromUrl "second-constructor/off")


    -- below more realistic and faster implementation with pregenerated router
    -- notice that toUrl used on router generates value without Maybe wrapping

    let fObj = FirstConstructor 8 true 9

        sObj = SecondConstructor false

        -- generting route for given type type:

        maybeRouter = router (Proxy :: Proxy UnionOfPrimitivePositionalValues)

    -- later we can use `router` (unrapped from Maybe) for url generation and url parsing:

    equal ("first-constructor/8/on/9") (toUrl router fObj)
    equal ("second-constructor/off") (toUrl router sObj)

    equal (Just fObj) (fromUrl router "first-constructor/8/on/9")
    equal (Just sObj) (fromUrl router "second-constructor/off")

    ```

  * netsted data type:

    ```purescript
    data PrimitivePositionalValues = PrimitivePositionalValues Int Boolean Int
    derive instance genericPrimitivePositionalValues :: Generic PrimitivePositionalValues

    data NestedStructures =
        FirstOuterConstructor UnionOfPrimitivePositionalValues
      | SecondOuterConstructor PrimitivePositionalValues
    derive instance genericNestedStructures :: Generic NestedStructures


    let fObj = FirstOuterConstructor (FirstConstructor 100 true 888)
        maybeRouter = router (Proxy :: Proxy NestedStructures)

    equal ("first-outer-constructor/first-constructor/100/on/888") (toUrl router fObj)
    equal (Just fObj) (fromUrl router "first-outer-constructor/first-constructor/100/on/888"))

    let sObj = SecondOuterConstructor (PrimitivePositionalValues 8 false 100)
    -- in case of single constructor (`PrimitivePositionalValues` has one),
    -- constructor name is omited in encoded url
    equal ("second-outer-constructor/8/off/100") (toUrl router sObj)
    equal (Just sObj) (fromUrl router "second-outer-constructor/8/off/100"))

    ```

  * records are converted into query (there is still some error handling missing...):

    ```purescript
    data SumWithInternalRecords
      = FirstConstructorWithRecord
          Int
          { int1 :: Int
          , string1 :: String
          }
      | SecondConstructorWithRecord
          String
          { int2 :: Int
          , string2 :: String
          , boolean2 :: Boolean
          }

    let fObj = FirstConstructorWithRecord 8 { int1: 8, string1: "string1" }
        sObj = SecondConstructorWithRecord "test" { int2: 8, string2: "string1", boolean2: false }

    equal "first-constructor-with-record/8/?string1=string1&int1=8" (toUrl router fObj)
    equal "second-constructor-with-record/test/?string2=string1&int2=8&boolean2=off" (toUrl router sObj))
    ```

  * data types isomorphic to `Data.Maybe.Maybe` are treated as optional values in queries:

    ```purescript
    data Optional a = Value a | Missing
    derive instance genericOptional :: Generic a => Generic (Optional a)

    newtype ParamsWithOptionals =
      ParamsWithOptionals
        { optParamString :: Optional String
        , optParamInt :: Optional Int
        , optParamBoolean :: Optional Boolean
        }
    derive instance genericParamsWithOptionals :: Generic ParamsWithOptionals

    ...

    let fObj = ParamsWithOptionals { optParamString: Value "test", optParamInt: Missing, optParamBoolean: Missing}
        sObj = ParamsWithOptionals { optParamString: Missing, optParamInt: Value 8, optParamBoolean: Missing}

    ...

    equal "?optParamString=test" (toUrl r fObj)
    equal (Just sObj) (fromUrl r "?optParamInt=8&optParamBoolean="))
    ```

## Which language and type constructs are covered?

Not all types and language constructs are covered by this library. Some are treated differently in records than in paths.

In general sum and product types are encoded as url path, but if you use records as a value in any field it will correspond to query parameters (there is no name collission detection implemented yet, so don't use the same names for records fields).
You can have arbitrary nesting of data types which will be translated into nested paths of constructors follwed by related values (only values of records fields are encoded in urls).

Here you have internal representation of language constructs which are already covered by this library:

   ```purescript

    type DataConstructorF r = { sigConstructor :: String, sigValues :: List r}

    data SigF r
      = SigProdF String (NonEmpty List (DataConstructorF r))
      | SigRecordF (NonEmpty List ({ recLabel :: String, recValue :: (SigRecValueF r) }))
      | SigBooleanF
      | SigIntF
      | SigStringF

    type JustConstructorName = String
    type NothingConstrtuctorName = String

    data SigRecValueF r
      = SigRecRequiredValueF r
      | SigRecOptionalValueF JustConstructorName NothingConstrtuctorName r
      | SigRecArrayF r
   ```

If you have any ideas how to nicely extend this set please let me know...

## Docs
You can check `test/Main.purs` for more examples... real docs comming soooooon ;-)

