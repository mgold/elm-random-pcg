module Random.Pcg exposing (Generator, Seed, bool, int, float, oneIn, sample, pair, list, maybe, choice, choices, frequency, map, map2, map3, map4, map5, andMap, filter, constant, andThen, minInt, maxInt, step, initialSeed2, initialSeed, independentSeed, fastForward, toJson, fromJson)

{-| Generate psuedo-random numbers and values, by constructing
[generators](#Generator) for them. There are a bunch of basic generators like
[`bool`](#bool) and [`int`](#int) that you can build up into fancier generators
with functions like [`list`](#list) and [`map`](#map).

You run a `Generator` by calling the [`step`](#step) function, which
also takes a random [`Seed`](#Seed), and passes back a new seed. You should
never use the same seed twice because you will get the same result! If you need
random values over time, you should store the most recent seed in your model. If
you have many separate models, you can give them all [independent
seeds](#independentSeed).

This is an implementation of [PCG](http://www.pcg-random.org/) by M. E. O'Neil,
and is not cryptographically secure.

# Getting Started
@docs initialSeed2, step

# Basic Generators
@docs Generator, bool, int, float, oneIn, sample

# Combining Generators
@docs pair, list, maybe, choice, choices, frequency

# Custom Generators
@docs constant, map, map2, map3, map4, map5, andMap, andThen, filter

# Working With Seeds
@docs Seed, initialSeed, independentSeed, fastForward, toJson, fromJson

# Constants
@docs minInt, maxInt
-}

import Bitwise
import Json.Encode
import Json.Decode


(&) =
    Bitwise.and


(<<) =
    Bitwise.shiftLeft


(>>>) =
    Bitwise.shiftRightLogical


{-| Private: A type used to represent 64-bit integers.
-}
type Int64
    = Int64 Int Int


{-| A `Generator` is like a recipe for generating certain random values. So a
`Generator Int` describes how to generate integers and a `Generator String`
describes how to generate strings.
-}
type Generator a
    = Generator (Seed -> ( a, Seed ))


{-| Generate a random value as specified by a given `Generator`, using a `Seed`
and returning a new one.

In the following example, we are trying to generate numbers between 0 and 100
with the `int 0 100` generator. Each time we call `generate` we need to provide
a seed. This will produce a random number and a *new* seed to use if we want to
run other generators later.

    (x, seed1) = step (int 0 100) seed0
    (y, seed2) = step (int 0 100) seed1
    (z, seed3) = step (int 0 100) seed2
    [x, y, z] -- [85, 0, 38]

Notice that we use different seeds on each line. This is important! If you reuse
the same seed, you get the same results.

    (x, _) = step (int 0 100) seed0
    (y, _) = step (int 0 100) seed0
    (z, _) = step (int 0 100) seed0
    [x,y,z] -- [85, 85, 85]

As you can see, threading seeds through many calls to `step` is tedious and
error-prone. That's why this library includes many functions to build more
complicated generators, allowing you to call `step` only a small number of
times.

Our example is best written as:

    (xs, newSeed) = step (list 3 <| int 0 100) seed0
    xs -- [85, 0, 38]

-}
step : Generator a -> Seed -> ( a, Seed )
step (Generator generator) seed =
    generator seed


{-| A `Seed` is the source of randomness in the whole system. It hides the
current state of the random number generator.

Generators, not seeds, are the primary data structure for generating random
values. Generators are much easier to chain and combine than functions that take
and return seeds. Creating and managing seeds should happen "high up" in your
program.
-}
type Seed
    = -- state and increment
      Seed Int64 Int64


{-| Take two integers to fully initialize the 64-bit state of the random
number generator. Only the least significant 32 bits of each integer matter, and
those bits should be as random as possible. The first argument is the high bits,
but if you're pulling from a random data source, it shouldn't matter.

You can generate and copy random integers to create a reproducible psuedo-random
generator.

    $ node
    > Math.floor(Math.random()*0xFFFFFFFF)
    227852860
    > Math.floor(Math.random()*0xFFFFFFFF)
    1498709020

    -- Elm
    seed0 : Seed
    seed0 = initialSeed2 227852860 1498709020

Alternatively, you can generate the random integers on page load and pass them
through a port. The program will be different every time.

    -- Elm
    port randomSeed : (Int, Int)

    seed0 : Seed
    seed0 = (uncurry initialSeed2) randomSeed

    -- JS
    Elm.fullscreen(Elm.ModuleName,
      {randomSeed: [Math.floor(Math.random()*0xFFFFFFFF),
                    Math.floor(Math.random()*0xFFFFFFFF)] })

Either way, you should initialize a random seed only once. After that, whenever
you use a seed, you'll get another one back.
-}
initialSeed2 : Int -> Int -> Seed
initialSeed2 stateHi stateLo =
    let
        incr =
            Int64 0x14057B7E 0xF767814F

        seed0 =
            Seed (Int64 0 0) incr

        (Seed state1 _) =
            next seed0

        state2 =
            add64 state1 <| Int64 (stateHi >>> 0) (stateLo >>> 0)
    in
        Seed state2 incr |> next


{-| Like `initialSeed2`, but takes only one integer. Mostly for compatibility
with core. The integer provided becomes the high bits of the seed.
-}
initialSeed : Int -> Seed
initialSeed i =
    initialSeed2 i 0


magicFactor : Int64
magicFactor =
    Int64 0x5851F42D 0x4C957F2D


{-| PRIVATE: derive the next seed by cranking the LCG
-}
next : Seed -> Seed
next (Seed state0 incr) =
    let
        state1 =
            mul64 state0 magicFactor

        state2 =
            add64 state1 incr
    in
        Seed state2 incr


{-| PRIVATE: obtain a psuedorandom 32-bit integer
-}
peel : Seed -> Int
peel (Seed (Int64 oldHi oldLo) _) =
    let
        -- get least sig. 32 bits of ((oldstate >> 18) ^ oldstate) >> 27
        xsHi =
            oldHi >>> 18

        xsLo =
            ((oldLo >>> 18) `Bitwise.or` (oldHi << 14)) >>> 0

        xsHi' =
            (xsHi `Bitwise.xor` oldHi) >>> 0

        xsLo' =
            (xsLo `Bitwise.xor` oldLo) >>> 0

        xorshifted =
            ((xsLo' >>> 27) `Bitwise.or` (xsHi' << 5)) >>> 0

        -- rotate xorshifted right a random amount, based on the most sig. 5 bits
        -- bits of the old state.
        rot =
            oldHi >>> 27

        rot2 =
            ((-rot >>> 0) & 31) >>> 0
    in
        ((xorshifted >>> rot) `Bitwise.or` (xorshifted << rot2)) >>> 0


{-| PRIVATE: Get a uniformly distributed 32 bit integer between [0, max).
-}
integer : Int -> Seed -> ( Int, Seed )
integer max seed0 =
    -- fast path for power of 2
    if ((max & (max - 1)) == 0) then
        ( peel seed0 & (max - 1) >>> 0, next seed0 )
    else
        let
            threshhold =
                -- essentially: period % max
                ((-max >>> 0) % max) >>> 0

            accountForBias : Seed -> ( Int, Seed )
            accountForBias seed =
                let
                    x =
                        peel seed

                    seedN =
                        next seed
                in
                    if x < threshhold then
                        -- in practice this recurses almost never
                        accountForBias seedN
                    else
                        ( x % max, seedN )
        in
            accountForBias seed0


{-| Generate 32-bit integers in a given range, inclusive.

    int 0 10   -- an integer between zero and ten
    int -5 5   -- an integer between -5 and 5

    int minInt maxInt  -- an integer in the widest range feasible

This function *can* produce values outside of the range [[`minInt`](#minInt),
[`maxInt`](#maxInt)] but sufficient randomness is not guaranteed.

*Performance note:* This function will be ~1.5x faster if the range (i.e. `max - min + 1`) is a power of two. The
effect will only be noticable if you are generating tens of thousands of random integers.

-}
int : Int -> Int -> Generator Int
int min max =
    Generator
        <| \seed0 ->
            if min == max then
                ( min, seed0 )
            else
                let
                    range =
                        abs (max - min) + 1

                    ( i, seed1 ) =
                        integer range seed0
                in
                    ( i + min, seed1 )


bit53 =
    9007199254740992.0


bit27 =
    134217728.0


{-| Generate floats in a given range. The following example is a generator
that produces numbers between 0 and 1.

    probability : Generator Float
    probability =
      float 0 1
-}
float : Float -> Float -> Generator Float
float min max =
    Generator
        <| \seed0 ->
            let
                -- Get 64 bits of randomness
                seed1 =
                    next seed0

                n0 =
                    peel seed0

                n1 =
                    peel seed1

                -- Get a uniformly distributed IEEE-754 double between 0.0 and 1.0
                hi =
                    toFloat (n0 & 0x03FFFFFF) * 1.0

                lo =
                    toFloat (n1 & 0x07FFFFFF) * 1.0

                val =
                    ((hi * bit27) + lo) / bit53

                -- Scale it into our range
                range =
                    abs (max - min)

                scaled =
                    val * range + min
            in
                ( scaled, next seed1 )


{-| A generator that produces a seed that is independent of any other seed in
the program. These seeds will generate their own unqiue sequences of random
values. They are useful when you need an unknown amount of randomness *later*
but can request only a fixed amount of randomness *now*.

Let's say you write a component that uses some randomness to initialize itself
and then never needs randomness again. You can easily write a `Generator
Component` by mapping over the generators it needs. But if component requires
randomness after initialization, it should keep its own independent seed, which
it can get by mapping over *this* generator.

    type alias Component = { seed : Seed }

    genComponent : Generator Component
    genComponent = map Component independentSeed

If you have a lot of components, you can initialize them like so:

    genComponents : List (Seed -> a) -> Generator (List a)
    genComponents constructors =
      list (List.length constructors) independentSeed
          |> map (List.map2 (<|) constructors)

The independent seeds are extremely likely to be distinct for all practical
purposes. However, it is not proven that there are no pathological cases.
-}
independentSeed : Generator Seed
independentSeed =
    Generator
        <| \seed0 ->
            let
                gen1 =
                    int 0 0xFFFFFFFF

                -- 2^32-1
                gen4 =
                    map4 (,,,) gen1 gen1 gen1 gen1

                ( ( a, b, c, d ), seed1 ) =
                    step gen4 seed0

                dOdd =
                    (d `Bitwise.or` 1) >>> 0

                seed2 =
                    Seed (Int64 a b) (Int64 c dOdd)
            in
                ( next seed2, next seed1 )


{-| Fast forward a seed the given number of steps, which may be negative (the
seed will be "rewound"). This allows a single seed to serve as a random-access
lookup table of random numbers. (To be sure no one else uses the seed, use
`step independentSeed` to split off your own.)

    diceRollTable : Int -> Int
    diceRollTable i =
      fastForward i mySeed |> step (int 1 6) |> fst
-}
fastForward : Int -> Seed -> Seed
fastForward delta0 (Seed state0 incr) =
    let
        one =
            Int64 0 1

        zero =
            Int64 0 0

        helper : Int64 -> Int64 -> Int64 -> Int64 -> Int -> Bool -> ( Int64, Int64 )
        helper accMult accPlus curMult curPlus delta repeat =
            let
                deltaOdd =
                    delta & 1 == 1

                accMult' =
                    if deltaOdd then
                        mul64 accMult curMult
                    else
                        accMult

                accPlus' =
                    if deltaOdd then
                        add64 (mul64 accPlus curMult) curPlus
                    else
                        accPlus

                curPlus' =
                    mul64 (add64 curMult one) curPlus

                curMult' =
                    mul64 curMult curMult

                newDelta =
                    delta >>> 1
            in
                if newDelta == 0 then
                    if delta0 < 0 && repeat then
                        helper accMult' accPlus' curMult' curPlus' -1 False
                    else
                        ( accMult', accPlus' )
                else
                    helper accMult' accPlus' curMult' curPlus' newDelta repeat

        ( accMultFinal, accPlusFinal ) =
            helper one zero magicFactor incr delta0 True

        state1 =
            mul64 accMultFinal state0 |> add64 accPlusFinal
    in
        Seed state1 incr


{-| Create a generator that produces boolean values with equal probability. This
example simulates flipping three coins and checking if they're all heads.

    threeHeads : Generator Bool
    threeHeads =
      map3 (\a b c -> a && b && c) bool bool bool
-}
bool : Generator Bool
bool =
    map ((==) 1) (int 0 1)


{-| The maximum value for randomly generated 32-bit ints.
-}
maxInt : Int
maxInt =
    2147483647


{-| The minimum value for randomly generated 32-bit ints.
-}
minInt : Int
minInt =
    -2147483648


{-| Create a pair of random values. A common use of this might be to generate
a point in a certain 2D space. Imagine we have a collage that is 400 pixels
wide and 200 pixels tall.

    randomPoint : Generator (Int,Int)
    randomPoint =
        pair (int -200 200) (int -100 100)

-}
pair : Generator a -> Generator b -> Generator ( a, b )
pair genA genB =
    map2 (,) genA genB


{-| Create a list of random values of a given length.

    floatList : Generator (List Float)
    floatList =
        list 10 (float 0 1)

    intList : Generator (List Int)
    intList =
        list 5 (int 0 100)

    intPairs : Generator (List (Int, Int))
    intPairs =
        list 10 <| pair (int 0 100) (int 0 100)
-}
list : Int -> Generator a -> Generator (List a)
list n (Generator generate) =
    Generator
        <| \seed ->
            listHelp [] n generate seed


listHelp : List a -> Int -> (Seed -> ( a, Seed )) -> Seed -> ( List a, Seed )
listHelp list n generate seed =
    if n < 1 then
        ( List.reverse list, seed )
    else
        let
            ( value, newSeed ) =
                generate seed
        in
            listHelp (value :: list) (n - 1) generate newSeed


{-| Create a generator that always produces the value provided. This is useful
when creating complicated chained generators and you need to handle a simple
case. It's also useful for the base case of recursive generators.
-}
constant : a -> Generator a
constant value =
    Generator (\seed -> ( value, seed ))


{-| Transform the values produced by a generator using a stateless function as a
callback.

These examples show how to generate letters based on a basic integer generator.

    lowercaseLetter : Generator Char
    lowercaseLetter =
      map (\n -> Char.fromCode (n + 97)) (int 0 25)

    uppercaseLetter : Generator Char
    uppercaseLetter =
      map (\n -> Char.fromCode (n + 65)) (int 0 25)

-}
map : (a -> b) -> Generator a -> Generator b
map func (Generator genA) =
    Generator
        <| \seed0 ->
            let
                ( a, seed1 ) =
                    genA seed0
            in
                ( func a, seed1 )


{-| Combine two generators. This is useful when you have a function with two
arguments that both need to be given random inputs.

    pointInCircle : Float -> Generator (Float, Float)
    pointInCircle radius =
      let
        r = float 0 radius
        theta = map degrees (float 0 360)
      in
        map2 (curry fromPolar) r theta

-}
map2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 func (Generator genA) (Generator genB) =
    Generator
        <| \seed0 ->
            let
                ( a, seed1 ) =
                    genA seed0

                ( b, seed2 ) =
                    genB seed1
            in
                ( func a b, seed2 )


{-| Combine three generators. This could be used to produce random colors.

    rgb : Generator Color.Color
    rgb =
      map3 Color.rgb (int 0 255) (int 0 255) (int 0 255)

    hsl : Generator Color.Color
    hsl =
      map3 Color.hsl (map degrees (float 0 360)) (float 0 1) (float 0 1)
-}
map3 : (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
map3 func (Generator genA) (Generator genB) (Generator genC) =
    Generator
        <| \seed0 ->
            let
                ( a, seed1 ) =
                    genA seed0

                ( b, seed2 ) =
                    genB seed1

                ( c, seed3 ) =
                    genC seed2
            in
                ( func a b c, seed3 )


{-| Combine four generators. This could be used to produce random transparent
colors.

    rgba : Generator Color.Color
    rgba =
      map4 Color.rgba (int 0 255) (int 0 255) (int 0 255) (float 0 1)
-}
map4 : (a -> b -> c -> d -> e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
map4 func (Generator genA) (Generator genB) (Generator genC) (Generator genD) =
    Generator
        <| \seed0 ->
            let
                ( a, seed1 ) =
                    genA seed0

                ( b, seed2 ) =
                    genB seed1

                ( c, seed3 ) =
                    genC seed2

                ( d, seed4 ) =
                    genD seed3
            in
                ( func a b c d, seed4 )


{-| Combine five generators.
-}
map5 : (a -> b -> c -> d -> e -> f) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f
map5 func (Generator genA) (Generator genB) (Generator genC) (Generator genD) (Generator genE) =
    Generator
        <| \seed0 ->
            let
                ( a, seed1 ) =
                    genA seed0

                ( b, seed2 ) =
                    genB seed1

                ( c, seed3 ) =
                    genC seed2

                ( d, seed4 ) =
                    genD seed3

                ( e, seed5 ) =
                    genE seed4
            in
                ( func a b c d e, seed5 )


{-| Map over any number of generators.

    randomPerson : Generator Person
    randomPerson =
      person `map` genFirstName
          `andMap` genLastName
          `andMap` genBirthday
          `andMap` genPhoneNumber
          `andMap` genAddress
          `andMap` genEmail
-}
andMap : Generator (a -> b) -> Generator a -> Generator b
andMap =
    map2 (<|)


{-| Chain random operations by providing a callback that accepts a
randomly-generated value. The random value can be used to drive more randomness.

The argument order matches `andThen`s from core, but requires the use of `flip`
to match `map` or work with `|>` chains.

This example shows how we can use `andThen` to generate a list of random values
*and* random length. Then we use `map` to apply a stateless function to that
list. Assume we already have `genName : Generator String` defined.

    authors : Generator String
    authors =
      int 1 5 -- number of authors
      |> (flip andThen) (\i -> list i genName)
      |> map (\ns ->
        case ns of
          [n] ->
            "Author: " ++ n
          n::ns ->
            "Authors: " ++ String.join ", " ns ++ " and " ++ n
          [] ->
            "This can't happen"
        )

If you find yourself calling `constant` in every branch of the callback, you can
probably use `map` instead.
-}
andThen : Generator a -> (a -> Generator b) -> Generator b
andThen (Generator generateA) callback =
    Generator
        <| \seed ->
            let
                ( result, newSeed ) =
                    generateA seed

                (Generator generateB) =
                    callback result
            in
                generateB newSeed


{-| Filter a generator so that all generated values satisfy the given predicate.

    evens : Generator Int
    evens =
      filter (\i -> i % 2 == 0) (int minInt maxInt)

**Warning:** If the predicate is unsatisfiable, the generator will not terminate, your
application will crash with a stack overflow, and you will be sad. You should
also avoid predicates that are merely very difficult to satisfy.

    badCrashingGenerator =
      filter (\_ -> False) anotherGenerator

    likelyCrashingGenerator =
      filter (\i -> i % 2000 == 0) (int minInt maxInt)
-}
filter : (a -> Bool) -> Generator a -> Generator a
filter predicate generator =
    generator
        `andThen` (\a ->
                    if predicate a then
                        constant a
                    else
                        filter predicate generator
                  )


{-| Produce `True` one-in-n times on average.

Do not pass a value less then one to this function.

    flippedHeads = oneIn 2
    rolled6 = oneIn 6
-}
oneIn : Int -> Generator Bool
oneIn n =
    map ((==) 1) (int 1 n)


{-| Given a list, choose an element uniformly at random. `Nothing` is only
produced if the list is empty.

    type Direction = North | South | East | West

    direction : Generator Direction
    direction =
      sample [North, South, East, West]
        |> map (Maybe.withDefault North)

-}
sample : List a -> Generator (Maybe a)
sample =
    let
        find k ys =
            case ys of
                [] ->
                    Nothing

                z :: zs ->
                    if k == 0 then
                        Just z
                    else
                        find (k - 1) zs
    in
        \xs -> map (\i -> find i xs) (int 0 (List.length xs - 1))


{-| Choose between two values with equal probability.

    type Flip = Heads | Tails

    coinFlip : Generator Flip
    coinFlip =
      choice Heads Tails
-}
choice : a -> a -> Generator a
choice x y =
    map
        (\b ->
            if b then
                x
            else
                y
        )
        bool


{-| Create a generator that chooses a generator from a list of generators
with equal probability.

**Warning:** Do not pass an empty list or your program will crash! In practice
this is usually not a problem since you pass a list literal.
-}
choices : List (Generator a) -> Generator a
choices gens =
    frequency <| List.map (\g -> ( 1, g )) gens


{-| Create a generator that chooses a generator from a list of generators
based on the provided weight. The likelihood of a given generator being
chosen is its weight divided by the total weight (which doesn't have to equal 1).

**Warning:** Do not pass an empty list or your program will crash! In practice
this is usually not a problem since you pass a list literal.
-}
frequency : List ( Float, Generator a ) -> Generator a
frequency pairs =
    let
        total =
            List.sum <| List.map (fst >> abs) pairs

        pick choices n =
            case choices of
                ( k, g ) :: rest ->
                    if n <= k then
                        g
                    else
                        pick rest (n - k)

                _ ->
                    Debug.crash "Empty list passed to Random.Pcg.frequency!"
    in
        float 0 total `andThen` pick pairs


{-| Produce `Just` a value on `True`, and `Nothing` on `False`.

You can use `bool` or `oneIn n` for the first argument.
-}
maybe : Generator Bool -> Generator a -> Generator (Maybe a)
maybe genBool genA =
    genBool
        `andThen` \b ->
                    if b then
                        map Just genA
                    else
                        constant Nothing


{-| Serialize a seed as a [JSON value](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Encode#Value)
to be sent out a port, stored in local storage, and so on. The seed can be
recovered using `fromJson`.

Do not inspect or change the resulting JSON value.
-}
toJson : Seed -> Json.Encode.Value
toJson (Seed (Int64 a b) (Int64 c d)) =
    Json.Encode.list <| List.map Json.Encode.int [ a, b, c, d ]


{-| A JSON decoder that can recover seeds encoded using `toJson`.

    Json.Decode.decodeValue fromJson (toJson mySeed) == Ok mySeed

If the JSON is an array of one or two integers, or just an integer, these will
be used to initialize a new seed. This can be useful when you sometimes have an
old seed and sometimes need a new one. The integers should be 32 random bits.
-}
fromJson : Json.Decode.Decoder Seed
fromJson =
    Json.Decode.oneOf
        [ Json.Decode.tuple4 (\a b c d -> Seed (Int64 a b) (Int64 c d))
            Json.Decode.int
            Json.Decode.int
            Json.Decode.int
            Json.Decode.int
        , Json.Decode.tuple2 initialSeed2 Json.Decode.int Json.Decode.int
        , Json.Decode.tuple1 initialSeed Json.Decode.int
        , Json.Decode.map initialSeed Json.Decode.int
        ]



---------------------------------------------------------------
-- Arithmetic helpers, because JS does not have 64-bit integers
---------------------------------------------------------------


mul32 : Int -> Int -> Int
mul32 a b =
    let
        ah =
            (a >>> 16) & 0xFFFF

        al =
            a & 0xFFFF

        bh =
            (b >>> 16) & 0xFFFF

        bl =
            b & 0xFFFF
    in
        (al * bl) + (((ah * bl + al * bh) << 16) >>> 0) |> Bitwise.or 0


mul64 : Int64 -> Int64 -> Int64
mul64 (Int64 aHi aLo) (Int64 bHi bLo) =
    let
        -- this is taken from a mutable implementation, so there are a lot of primes.
        c1 =
            (aLo >>> 16) * (bLo & 0xFFFF) >>> 0

        c0 =
            (aLo & 0xFFFF) * (bLo >>> 16) >>> 0

        lo =
            ((aLo & 0xFFFF) * (bLo & 0xFFFF)) >>> 0

        hi =
            ((aLo >>> 16) * (bLo >>> 16)) + ((c0 >>> 16) + (c1 >>> 16)) >>> 0

        c0' =
            (c0 << 16) >>> 0

        lo' =
            (lo + c0') >>> 0

        hi' =
            if (lo' >>> 0) < (c0' >>> 0) then
                (hi + 1) >>> 0
            else
                hi

        c1' =
            (c1 << 16) >>> 0

        lo'' =
            (lo' + c1') >>> 0

        hi'' =
            if (lo'' >>> 0) < (c1' >>> 0) then
                (hi' + 1) >>> 0
            else
                hi'

        hi''' =
            (hi'' + mul32 aLo bHi) >>> 0

        hi'''' =
            (hi''' + mul32 aHi bLo) >>> 0
    in
        Int64 hi'''' lo''


add64 : Int64 -> Int64 -> Int64
add64 (Int64 aHi aLo) (Int64 bHi bLo) =
    let
        hi =
            (aHi + bHi) >>> 0

        lo =
            (aLo + bLo) >>> 0

        hi' =
            if ((lo >>> 0) < (aLo >>> 0)) then
                (hi + 1) `Bitwise.or` 0
            else
                hi
    in
        Int64 hi' lo
