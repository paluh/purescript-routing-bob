# purescript-routing-bob

Let me introduce to you Bob The Router - simple (but usable) example of partial bidirectional ~~bobs~~ routes generator based on __purescript-boomerang__ and purescript generics. Bob is dirty and fast like quick hack or implementation prototype ;-)

## Installation

```shell
bower install purescript-routing-bob
```

## Limitations

It is called `bob`, because it contains arbitrary, nonconfigurable and partial routes generators based on `Generic` instances.
When it will grow up maybe it will became `purescript-boomerang-routing`.

Currently you can generate routes only for some subset of purescript types - if you want extend this set and have proposition how to encode others I'm really open to merge pull requests.

## Usage

Just to give you a hit what this library does, let's copy simple test here (sorry for long data types and constructor names):

```purescript
import Data.Generic (class Generic)
import Routing.Bob (bob)
import Type.Proxy (Proxy(..))


data UnionOfPrimitivePositionalValues =
    FirstConstructor Int Boolean Int
  | SecondConstructor Boolean
derive instance genericUnionOfPrimitivePositionalValues :: Generic UnionOfPrimitivePositionalValues

-- and related tests' lines

let fObj = FirstConstructor 8 true 9

    sObj = SecondConstructor false

    -- generting route from type
    maybeRoute = bob (Proxy :: Proxy UnionOfPrimitivePositionalValues)

-- later we can use this `route` for url generation and url parsing:

equal (Just "firstconstructor/8/on/9") (serialize route fObj)

equal (Just "secondconstructor/off") (serialize route sObj)

equal (Just fObj) (parse route "firstconstructor/8/on/9")

equal (Just sObj) (parse route "secondconstructor/off")

```

You can check `test/Main.purs` for more examples... real docs comming soon ;-)
