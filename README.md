# purescript-routing-bob

Let me introduce to you Bob The Router - simple (but usable) example of partial bidirectional ~~bobs~~ routes generator based on __purescript-boomerang__ and __purescript-generics__. Bob is dirty and fast like quick hack or implementation prototype ;-)

## Installation

```shell
bower install purescript-routing-bob
```

## Limitations

It is called `bob`, because it contains arbitrary, nonconfigurable and partial routes generators based on `Generic` instances.
When it will grow up maybe it will became `purescript-boomerang-routing`.

Currently you can generate routes only for some subset of purescript types - if you want extend this set and have proposition how to encode others I'm really open to merge pull requests.

## Usage

Just to give you a hint what this library does, let's copy some tests' fragments (sorry for long data types and constructor names):

  * simple, union type:

    ```purescript
    import Data.Generic (class Generic)
    import Routing.Bob (bob)
    import Type.Proxy (Proxy(..))


    data UnionOfPrimitivePositionalValues =
        FirstConstructor Int Boolean Int
      | SecondConstructor Boolean
    derive instance genericUnionOfPrimitivePositionalValues :: Generic UnionOfPrimitivePositionalValues


    -- and related tests' lines

    equal (Just "first-constructor/8/on/9") (toUrl (FirstConstructor 8 true 9))
    equal (Just "second-constructor/off") (toUrl (SecondConstructor false))

    equal (Just (FirstConstructor 8 true 9)) (fromUrl "first-constructor/8/on/9")
    equal (Just (SecondConstructor false)) (fromUrl "second-constructor/off")


    -- below more realistic and faster implementation with pregenerated router

    let fObj = FirstConstructor 8 true 9

        sObj = SecondConstructor false

        -- generting route for given type type:

        maybeRoute = bob (Proxy :: Proxy UnionOfPrimitivePositionalValues)

    -- later we can use this `route` for url generation and url parsing:

    equal (Just "first-constructor/8/on/9") (serialize route fObj)
    equal (Just "second-constructor/off") (serialize route sObj)

    equal (Just fObj) (parse route "first-constructor/8/on/9")
    equal (Just sObj) (parse route "second-constructor/off")

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

    equal (Just "first-outer-constructor/first-constructor/100/on/888") (tUrl fObj)
    equal (Just fObj) (fromUrl "first-outer-constructor/first-constructor/100/on/888"))

    let sObj = SecondOuterConstructor (PrimitivePositionalValues 8 false 100)
    -- in case of single constructor (`PrimitivePositionalValues` has one),
    -- constructor name is omited in encoded url
    equal (Just "second-outer-constructor/8/off/100") (serialize route sObj)
    equal (Just sObj) (parse route "second-outer-constructor/8/off/100"))

    ```

You can check `test/Main.purs` for more examples... real docs comming soon ;-)
