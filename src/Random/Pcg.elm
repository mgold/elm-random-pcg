module Random.Pcg exposing (Generator, Seed, bool, int, float, oneIn, sample, pair, list, maybe, choice, choices, frequency, map, map2, map3, map4, map5, andMap, filter, constant, andThen, minInt, maxInt, step, initialSeed2, initialSeed, independentSeed)

{-| This is a stubbed-out copy of Random.Pcg. It's not actually random, but it's meant to tell us if
it's the bottleneck of project fuzzball.

@docs Generator, Seed, bool, int, float, oneIn, sample, pair, list, maybe, choice, choices, frequency, map, map2, map3, map4, map5, andMap, filter, constant, andThen, minInt, maxInt, step, initialSeed2, initialSeed, independentSeed
-}


{-| -}
type Generator a
    = Generator (Seed -> ( a, Seed ))


{-|
-}
step : Generator a -> Seed -> ( a, Seed )
step (Generator generator) seed =
    generator seed


{-|
-}
type alias Seed =
    ()


{-|
-}
initialSeed2 : Int -> Int -> Seed
initialSeed2 stateHi stateLo =
    ()


{-|
-}
initialSeed : Int -> Seed
initialSeed i =
    ()


{-|
-}
int : Int -> Int -> Generator Int
int min max =
    constant min


{-|
-}
float : Float -> Float -> Generator Float
float min max =
    Generator <| \seed0 -> ( min + 0.0000001, () )


{-|
-}
independentSeed : Generator Seed
independentSeed =
    constant ()


{-|
-}
bool : Generator Bool
bool =
    constant True


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
    constant False


{-| Given a list, choose an element uniformly at random. `Nothing` is only
produced if the list is empty.

    type Direction = North | South | East | West

    direction : Generator Direction
    direction =
      sample [North, South, East, West]
        |> map (Maybe.withDefault North)

-}
sample : List a -> Generator (Maybe a)
sample xs =
    constant Nothing


{-| Choose between two values with equal probability.

    type Flip = Heads | Tails

    coinFlip : Generator Flip
    coinFlip =
      choice Heads Tails
-}
choice : a -> a -> Generator a
choice x y =
    constant x


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


{-|
-}
maybe : Generator Bool -> Generator a -> Generator (Maybe a)
maybe genBool genA =
    constant Nothing
