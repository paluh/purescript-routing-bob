# purescript-routing-bob

Let me introduce to you Bob The Router (known also as "Pendulum Bob") - simple (but usable) example of partial bidirectional ~~bobs~~ routes generator based on __purescript-boomerang__ and __purescript-generics__.

 Bob is dirty and fast like quick hack or implementation prototype ;-)

## Installation

```shell
bower install purescript-routing-bob
```

## Limitations

It is called `Just bob`, because it contains arbitrary, nonconfigurable and partial routes generators based on `Generic` instances.
When it will grow up maybe it will became `purescript-boomerang-routing`.

Currently you can generate routes only for some subset of purescript types - if you want to extend this set or have proposition how to encode other types, I'm really open to merge pull requests.

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

You can check `test/Main.purs` for more examples... real docs comming soooooon ;-)
