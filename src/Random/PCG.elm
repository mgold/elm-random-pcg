module Random.PCG
    ( Generator, Seed
    , bool, int, float
    , list, pair
    , map, map2, map3, map4, map5
    , constant, andThen
    , minInt, maxInt
    , generate, initialSeed, split
    )
  where

{-| This library helps you generate pseudo-random values.

This library is all about building [`generators`](#Generator) for whatever
type of values you need. There are a bunch of primitive generators like
[`bool`](#bool) and [`int`](#int) that you can build up into fancier
generators with functions like [`list`](#list) and [`map`](#map).

You use a `Generator` by running the [`generate`](#generate) function, which
also takes a random seed, and passes back a new seed. You should never use the
same seed twice because you will get the same result! If you need random values
across many frames, you should store the most recent seed in your model.

*Note:* This is an implementation of the Portable Combined Generator of
L'Ecuyer for 32-bit computers. It is almost a direct translation from the
[System.Random](http://hackage.haskell.org/package/random-1.0.1.1/docs/System-Random.html)
module. It has a period of roughly 2.30584e18.

# Generators
@docs Generator

# Primitive Generators
@docs bool, int, float

# Data Structure Generators
@docs pair, list

# Custom Generators
@docs constant, map, map2, map3, map4, map5, andThen

# Run a Generator
@docs generate, Seed, initialSeed, split

# Constants
@docs maxInt, minInt

-}

import Basics exposing (..)
import List exposing ((::))

import Native.Random.PCG


{-| Create a generator that produces boolean values. The following example
simulates a coin flip that may land heads or tails.

    type Flip = Heads | Tails

    coinFlip : Generator Flip
    coinFlip =
        map (\b -> if b then Heads else Tails) bool
-}
bool : Generator Bool
bool =
  map ((==) 1) (int 0 1)


{-| Generate 32-bit integers in a given range.

    int 0 10   -- an integer between zero and ten
    int -5 5   -- an integer between -5 and 5

    int minInt maxInt  -- an integer in the widest range feasible

This function *can* produce values outside of the range [[`minInt`](#minInt),
[`maxInt`](#maxInt)] but sufficient randomness is not guaranteed.
-}
int : Int -> Int -> Generator Int
int a b =
  Generator <| Native.Random.PCG.intt a b


{-| The maximum value for randomly generated 32-bit ints. -}
maxInt : Int
maxInt =
  0xFFFFFFFF -- 2^32 - 1


{-| The minimum value for randomly generated 32-bit ints. -}
minInt : Int
minInt =
  0


{-| Generate floats in a given range. The following example is a generator
that produces decimals between 0 and 1.

    probability : Generator Float
    probability =
        float 0 1
-}
float : Float -> Float -> Generator Float
float a b =
  Generator <| Native.Random.PCG.floatt a b


-- DATA STRUCTURES

{-| Create a pair of random values. A common use of this might be to generate
a point in a certain 2D space. Imagine we have a collage that is 400 pixels
wide and 200 pixels tall.

    randomPoint : Generator (Int,Int)
    randomPoint =
        pair (int -200 200) (int -100 100)

-}
pair : Generator a -> Generator b -> Generator (a,b)
pair genA genB =
  map2 (,) genA genB


{-| Create a list of random values.

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
  Generator <| \seed ->
    listHelp [] n generate seed


listHelp : List a -> Int -> (Seed -> (a,Seed)) -> Seed -> (List a, Seed)
listHelp list n generate seed =
  if n < 1 then
    (List.reverse list, seed)
  else
    let
      (value, newSeed) =
        generate seed
    in
      listHelp (value :: list) (n-1) generate newSeed


{-| Create a generator that always produces the value provided. This is useful
when creating complicated chained generators and you need to handle a simple
case.
-}
constant : a -> Generator a
constant value =
  Generator (\seed -> (value, seed))


{-| Transform the values produced by a generator. The following examples show
how to generate booleans and letters based on a basic integer generator.

    bool : Generator Bool
    bool =
      map ((==) 1) (int 0 1)

    lowercaseLetter : Generator Char
    lowercaseLetter =
      map (\n -> Char.fromCode (n + 97)) (int 0 25)

    uppercaseLetter : Generator Char
    uppercaseLetter =
      map (\n -> Char.fromCode (n + 65)) (int 0 25)

-}
map : (a -> b) -> Generator a -> Generator b
map func (Generator genA) =
  Generator <| \seed0 ->
    let
      (a, seed1) = genA seed0
    in
      (func a, seed1)


{-| Combine two generators.

This function is used to define things like [`pair`](#pair) where you want to
put two generators together.

    pair : Generator a -> Generator b -> Generator (a,b)
    pair genA genB =
      map2 (,) genA genB

-}
map2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 func (Generator genA) (Generator genB) =
  Generator <| \seed0 ->
    let
      (a, seed1) = genA seed0
      (b, seed2) = genB seed1
    in
      (func a b, seed2)


{-| Combine three generators. This could be used to produce random colors.

    import Color

    rgb : Generator Color.Color
    rgb =
      map3 Color.rgb (int 0 255) (int 0 255) (int 0 255)

    hsl : Generator Color.Color
    hsl =
      map3 Color.hsl (map degrees (int 0 360)) (float 0 1) (float 0 1)
-}
map3 : (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
map3 func (Generator genA) (Generator genB) (Generator genC) =
  Generator <| \seed0 ->
    let
      (a, seed1) = genA seed0
      (b, seed2) = genB seed1
      (c, seed3) = genC seed2
    in
      (func a b c, seed3)


{-| Combine four generators.

    import Color

    rgba : Generator Color.Color
    rgba =
      map4 Color.rgb (int 0 255) (int 0 255) (int 0 255) (float 0 1)
-}
map4 : (a -> b -> c -> d -> e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
map4 func (Generator genA) (Generator genB) (Generator genC) (Generator genD) =
  Generator <| \seed0 ->
    let
      (a, seed1) = genA seed0
      (b, seed2) = genB seed1
      (c, seed3) = genC seed2
      (d, seed4) = genD seed3
    in
      (func a b c d, seed4)


{-| Combine five generators.
-}
map5 : (a -> b -> c -> d -> e -> f) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f
map5 func (Generator genA) (Generator genB) (Generator genC) (Generator genD) (Generator genE) =
  Generator <| \seed0 ->
    let
      (a, seed1) = genA seed0
      (b, seed2) = genB seed1
      (c, seed3) = genC seed2
      (d, seed4) = genD seed3
      (e, seed5) = genE seed4
    in
      (func a b c d e, seed5)


{-| Chain random operations, threading through the seed. In the following
example, we will generate a random letter by putting together uppercase and
lowercase letters.

    letter : Generator Char
    letter =
      bool `andThen` \b ->
        if b then uppercaseLetter else lowercaseLetter

    -- bool : Generator Bool
    -- uppercaseLetter : Generator Char
    -- lowercaseLetter : Generator Char
-}
andThen : Generator a -> (a -> Generator b) -> Generator b
andThen (Generator generate) callback =
  Generator <| \seed ->
    let
      (result, newSeed) =
        generate seed

      (Generator genB) =
        callback result
    in
      genB newSeed


{-| A `Generator` is like a recipe for generating certain random values. So a
`Generator Int` describes how to generate integers and a `Generator String`
describes how to generate strings.

To actually *run* a generator and produce the random values, you need to use
functions like [`generate`](#generate) and [`initialSeed`](#initialSeed).
-}
type Generator a =
    Generator (Seed -> (a, Seed))


{-| A `Seed` is the source of randomness in this whole system. Whenever
you want to use a generator, you need to supply a seed. You will get back a new
seed, which you must use to generate new random numbers.
-}
type Seed = Seed


{-| Generate a random value as specified by a given `Generator`.

In the following example, we are trying to generate a number between 0 and 100
with the `int 0 100` generator. Each time we call `generate` we need to provide
a seed. This will produce a random number and a *new* seed to use if we want to
run other generators later.

So here it is done right, where we get a new seed from each `generate` call and
thread that through.

    seed0 = initialSeed 31415

    -- generate (int 0 100) seed0 ==> (42, seed1)
    -- generate (int 0 100) seed1 ==> (31, seed2)
    -- generate (int 0 100) seed2 ==> (99, seed3)

Notice that we use different seeds on each line. This is important! If you use
the same seed, you get the same results.

    -- generate (int 0 100) seed0 ==> (42, seed1)
    -- generate (int 0 100) seed0 ==> (42, seed1)
    -- generate (int 0 100) seed0 ==> (42, seed1)
-}
generate : Generator a -> Seed -> (a, Seed)
generate (Generator generator) seed =
    generator seed


{-| Create a &ldquo;seed&rdquo; of randomness which makes it possible to
generate random values. If you use the same seed many times, it will result
in the same thing every time!

You should call this function only once in your entire program. If you call it
on sequential numbers (like the current time over a few seconds), each seed will
generate nearly-identical results. To obtain a good starting seed value, run
`Math.floor(Math.random()*0xFFFFFFFF)` in a JavaScript console. If you want each
run of your program to be unique, you can pass this value in through a port.

    -- DON'T DO THIS: it generates `True` ten times!
    List.map (\i -> generate bool (initialSeed i) |> fst) [0..10]

    -- DO THIS INSTEAD: it generates a reasonable list, and keeps the new seed.
    (bools, newSeed) = generate (list 10 bool) (initialSeed 999999999)
-}
initialSeed : Int -> Seed
initialSeed s =
  Native.Random.PCG.initialSeed2 0 s


{-| Split a seed into two new seeds. Each seed will generate different random
numbers.

Let's say you have have many independent components which will each want to
generate many random numbers. After splitting a seed, you can pass one of the
new seeds to a component, and keep the other to repeat the process.

    makeComponents : Seed -> List (Seed -> Component) -> (List Component, Seed)
    makeComponents seed constructors =
      case constructors of
        [] ->
          ([], seed)

        c::cs ->
          let
            (seed1, seed2) = split seed
            (tail, seed3) = makeComponents seed2 cs
          in
            (c seed1 :: tail, seed3)

-}
split : Seed -> (Seed, Seed)
split = Native.Random.PCG.split
