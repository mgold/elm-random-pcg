module Random.PCG
  ( Generator, Seed
  , bool, int, float
  , list, pair
  , map, map2, map3, map4, map5, andMap
  , constant, andThen
  , minInt, maxInt
  , generate, initialSeed2, initialSeed, split, fastForward
  )
  where

{-| Generate psuedo-random numbers and values, by constructing
[generators](#Generator) for them. There are a bunch of basic generators like
[`bool`](#bool) and [`int`](#int) that you can build up into fancier generators
with functions like [`list`](#list) and [`map`](#map).

You run a `Generator` by calling the [`generate`](#generate) function, which
also takes a random [`Seed`](#Seed), and passes back a new seed. You should
never use the same seed twice because you will get the same result! If you need
random values across many frames, you should store the most recent seed in your
model. If you need several independent models, you can [`split`](#split) seeds
into more seeds.

# Generators
@docs Generator, generate

# Basic Generators
@docs bool, int, float

# Data Structure Generators
@docs pair, list

# Custom Generators
@docs constant, map, map2, map3, map4, map5, andMap, andThen

# Seeds
@docs Seed, initialSeed2, initialSeed, split, fastForward

# Constants
@docs maxInt, minInt
-}


import Bitwise

(&) = Bitwise.and
(<<) = Bitwise.shiftLeft
(>>>) = Bitwise.shiftRightLogical

type Int64 = Int64 Int Int

{-| A `Generator` is like a recipe for generating certain random values. So a
`Generator Int` describes how to generate integers and a `Generator String`
describes how to generate strings.
-}
type Generator a =
    Generator (Seed -> (a, Seed))


{-| Generate a random value as specified by a given `Generator`, using a `Seed`
and returning a new one.

In the following example, we are trying to generate numbers between 0 and 100
with the `int 0 100` generator. Each time we call `generate` we need to provide
a seed. This will produce a random number and a *new* seed to use if we want to
run other generators later.

    seed0 = initialSeed2 227852860 1498709020
    (x, seed1) = generate (int 0 100) seed0
    (y, seed2) = generate (int 0 100) seed1
    (z, seed3) = generate (int 0 100) seed2
    [x, y, z] -- [85, 0, 38]

Notice that we use different seeds on each line. This is important! If you use
the same seed, you get the same results.

    (x, seed1) = generate (int 0 100) seed0
    (y, seed2) = generate (int 0 100) seed0
    (z, seed3) = generate (int 0 100) seed0
    [x,y,z] -- [85, 85, 85]

The rest of the library is how to make generators for any kind of value you
like, and how to create and manage random seeds.
-}
generate : Generator a -> Seed -> (a, Seed)
generate (Generator generator) seed =
    generator seed


{-| A `Seed` is the source of randomness in the whole system. Whenever you want
to use a generator, you need to supply a seed. You will get back a new seed,
which you must use to generate new random numbers. If you use the same seed many
times, it will result in the same thing every time!

Seeds are created by providing initial values, which should happen only once per
program. Although this library is more forgiving of poor seed choice than core,
the best random seeds are drawn uniformly at random from the space of possible
seeds. You can get a good starting value by running
`Math.floor(Math.random()*0xFFFFFFFF)` in a JavaScript console.
-}
type Seed = Seed Int64 Int64 -- state and increment


{-| Take two integers to fully initialize the 64-bit state of the random
number generator. Only the least significant 32 bits of each integer matter.

One can generate values at random and copy them to create a reproducible random
generator.

    $ node
    > Math.floor(Math.random()*0xFFFFFFFF)
    227852860
    > Math.floor(Math.random()*0xFFFFFFFF)
    1498709020

    -- Elm
    seed0 : Seed
    seed0 = initialSeed2 227852860 1498709020

Alternatively, we can generate the randomized values dynamically and pass them
through a port. Out program will be different every time.

    -- Elm
    port randomSeed : (Int, Int)

    seed0 : Seed
    seed0 = (uncurry initialSeed2) randomSeed

    -- JS
    Elm.fullscreen(Elm.ModuleName,
      {randomSeed: [Math.floor(Math.random()*0xFFFFFFFF), Math.floor(Math.random()*0xFFFFFFFF)] })
-}
initialSeed2 : Int -> Int -> Seed
initialSeed2 stateHi stateLo =
  let
    zero = Int64 0 0
    incr = Int64 0x14057b7e 0xf767814f
    seed0 = Seed zero incr
    (Seed state1 _) = next seed0
    state2 = add64 state1 <| Int64 (stateHi>>>0) (stateLo>>>0)
  in
    Seed state2 incr |> next


{-| Like `initialSeed2`, but takes only one integer. Mostly for compatibility
with core.
-}
initialSeed : Int -> Seed
initialSeed = initialSeed2 0


magicFactor = Int64 0x5851f42d 0x4c957f2d

-- derive the next seed by cranking the LCG
next : Seed -> Seed
next (Seed state0 incr) =
  let
    state1 = mul64 state0 magicFactor
    state2 = add64 state1 incr
  in
    Seed state2 incr


-- obtain a psuedorandom 32-bit integer
peel : Seed -> Int
peel (Seed (Int64 oldHi oldLo) _) =
  let
    -- get least sig. 32 bits of ((oldstate >> 18) ^ oldstate) >> 27
    xsHi = oldHi >>> 18
    xsLo = ((oldLo >>> 18) `Bitwise.or` (oldHi << 14)) >>> 0
    xsHi' = (xsHi `Bitwise.xor` oldHi) >>> 0
    xsLo' = (xsLo `Bitwise.xor` oldLo) >>> 0
    xorshifted = ((xsLo' >>> 27) `Bitwise.or` (xsHi' << 5)) >>> 0

    -- rotate xorshifted right a random amount, based on the most sig. 5 bits
    -- bits of the old state.
    rot = oldHi >>> 27
    rot2 = ((-rot >>> 0) & 31) >>> 0
  in
    ((xorshifted >>> rot) `Bitwise.or` (xorshifted << rot2)) >>> 0


  -- Get a uniformly distributed 32 bit integer between [0, max).
integer : Int -> Seed -> (Int, Seed)
integer max seed0 =
  -- fast path for power of 2
  if ((max & (max - 1)) == 0)
  then (peel seed0 & (max - 1) >>> 0, next seed0)
  else
    let
      threshhold = ((-max >>> 0) % max) >>> 0 -- essentially: period % max
      accountForBias : Seed -> (Int, Seed)
      accountForBias seed =
        -- in practice this recurses almost never
        let x = peel seed
            seedN = next seed
        in if x < threshhold then accountForBias seedN else (x % max, seedN)
    in
      accountForBias seed0

{-| Generate 32-bit integers in a given range.

    int 0 10   -- an integer between zero and ten
    int -5 5   -- an integer between -5 and 5

    int minInt maxInt  -- an integer in the widest range feasible

This function *can* produce values outside of the range [[`minInt`](#minInt),
[`maxInt`](#maxInt)] but sufficient randomness is not guaranteed.
-}
int : Int -> Int -> Generator Int
int min max =
  Generator <| \seed0 ->
    if min == max
    then (min, seed0)
    else
      let
        range = abs(max - min) + 1
        (i, seed1) = integer range seed0
      in
        (i+min, seed1)

bit53 = 9007199254740992.0
bit27 = 134217728.0

{-| Generate floats in a given range. The following example is a generator
that produces decimals between 0 and 1.

    probability : Generator Float
    probability =
        float 0 1
-}
float : Float -> Float -> Generator Float
float min max =
  Generator <| \seed0 ->
    let
      -- Get 64 bits of randomness
      seed1 = next seed0
      n0 = peel seed0
      n1 = peel seed1

      -- Get a uniformly distributed IEEE-754 double between 0.0 and 1.0
      hi = toFloat (n0 & 0x03ffffff) * 1.0
      lo = toFloat (n1 & 0x07ffffff) * 1.0
      val = ((hi * bit27) + lo) / bit53

      -- Scale it into our range
      range = abs(max - min)
      scaled = val * range + min
    in
      (scaled, next seed1)


{-| Split a seed into two new seeds. Each seed will generate different random
numbers. Splitting is a reproducible operation; just like generating numbers, it
will be the same every time.

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
split seed0 =
  let
    gen1 = int minInt maxInt
    gen4 = map4 (,,,) gen1 gen1 gen1 gen1
    ((a,b,c,d), seed1) = generate gen4 seed0
    dOdd = (d `Bitwise.or` 1) >>> 0
    seed2 = Seed (Int64 a b) (Int64 c dOdd)
  in
    (next seed1, next seed2)


{-| Fast forward a seed the given number of steps, which may be negative (the
seed will be "rewound"). This allows a single seed to serve as a random-access
lookup table of random numbers. (To be sure no one else uses the seed,
[`split`](#split) off your own).

    diceRollTable : Int -> Int
    diceRollTable i =
      fastForward i mySeed |> generate (int 1 6) |> fst
-}
fastForward : Int -> Seed -> Seed
fastForward delta0 (Seed state0 incr) =
  let
    one = Int64 0 1
    zero = Int64 0 0

    helper : Int64 -> Int64 -> Int64 -> Int64 -> Int -> Bool -> (Int64, Int64)
    helper accMult accPlus curMult curPlus delta repeat =
      let
        deltaOdd = delta & 1 == 1
        accMult' = if deltaOdd then mul64 accMult curMult else accMult
        accPlus' = if deltaOdd then add64 (mul64 accPlus curMult) curPlus else accPlus
        curPlus' = mul64 (add64 curMult one) curPlus
        curMult' = mul64 curMult curMult
        newDelta = delta >>> 1
      in
        if newDelta == 0 then
          if delta0 < 0 && repeat then
            helper accMult' accPlus' curMult' curPlus' -1 False
          else
            (accMult', accPlus')
        else
          helper accMult' accPlus' curMult' curPlus' newDelta repeat

    (accMultFinal, accPlusFinal) = helper one zero magicFactor incr delta0 True
    state1 = mul64 accMultFinal state0 |> add64 accPlusFinal
  in
    Seed state1 incr


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


{-| The maximum value for randomly generated 32-bit ints. -}
maxInt : Int
maxInt =
  0xFFFFFFFF -- 2^32 - 1


{-| The minimum value for randomly generated 32-bit ints. -}
minInt : Int
minInt =
  0

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

{-| Map over any number of generators.

    randomPerson =
      person `map` genFirstName
        `andMap` genLastName
        `andMap` genBirthday
        `andMap` genPhoneNumber
-}
andMap : Generator (a -> b) -> Generator a -> Generator b
andMap =
  map2 (<|)


{-| Chain random operations by providing a callback that accepts a
randomly-generated value. This all happens inside the generator, and the seed is
managed for you.

In the following example, we will generate a random letter by putting together
uppercase and lowercase letters.

    letter : Generator Char
    letter =
      bool `andThen` \b ->
        if b then genUppercaseLetter else genLowercaseLetter
-}
andThen : Generator a -> (a -> Generator b) -> Generator b
andThen (Generator generateA) callback =
  Generator <| \seed ->
    let
      (result, newSeed) =
        generateA seed

      (Generator generateB) =
        callback result
    in
      generateB newSeed


---------------------------------------------------------------
-- Arithmetic helpers, because JS does not have 64-bit integers
---------------------------------------------------------------

mul32 : Int -> Int -> Int
mul32 a b =
  let
    ah = (a >>> 16) & 0xffff
    al = a & 0xffff
    bh = (b >>> 16) & 0xffff
    bl = b & 0xffff
  in
    (al * bl) + (((ah * bl + al * bh) << 16) >>> 0) |> Bitwise.or 0

mul64 : Int64 -> Int64 -> Int64
mul64 (Int64 aHi aLo) (Int64 bHi bLo) =
  let
    -- this is taken from a mutable implementation, so there are a lot of primes.
    c1 = (aLo >>> 16) * (bLo & 0xffff) >>> 0
    c0 = (aLo & 0xffff) * (bLo >>> 16) >>> 0

    lo = ((aLo & 0xffff) * (bLo & 0xffff)) >>> 0
    hi = ((aLo >>> 16) * (bLo >>> 16)) + ((c0 >>> 16) + (c1 >>> 16)) >>> 0

    c0' = (c0 << 16) >>> 0
    lo' = (lo + c0') >>> 0
    hi' = if (lo' >>> 0) < (c0' >>> 0) then (hi + 1) >>> 0 else hi

    c1' = (c1 << 16) >>> 0
    lo'' = (lo' + c1') >>> 0
    hi'' = if (lo'' >>> 0) < (c1' >>> 0) then (hi' + 1) >>> 0 else hi'

    hi''' = (hi'' + mul32 aLo  bHi) >>> 0
    hi'''' = (hi''' + mul32 aHi bLo) >>> 0

  in
    Int64 hi'''' lo''


add64 : Int64 -> Int64 -> Int64
add64 (Int64 aHi aLo) (Int64 bHi bLo) =
  let
    hi = (aHi + bHi) >>> 0
    lo = (aLo + bLo) >>> 0
    hi' = if ((lo >>> 0) < (aLo >>> 0)) then  (hi + 1) `Bitwise.or` 0 else hi
  in
    Int64 hi' lo
