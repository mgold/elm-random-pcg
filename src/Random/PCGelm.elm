module PCGelm where

{-|
Random.PCG for Elm
Max Goldstein

PCG by M. E. O'Neil: http://www.pcg-random.org/
JS port by Thom Chiovoloni (MIT license): https://github.com/thomcc/pcg-random

-}

import Bitwise

(&) = Bitwise.and
(<<) = Bitwise.shiftLeft
(>>>) = Bitwise.shiftRightLogical

type Int64 = Int64 Int Int

type Seed = Seed Int64 Int64 -- state and increment

initialSeed2 : Int -> Int -> Seed
initialSeed2 stateHi stateLo =
  let
    zero = Int64 0 0
    inc = Int64 0x14057b7e 0xf767814f
    seed0 = Seed zero inc
    (Seed state1 _) = next seed0
    state2 = add64 state1 <| Int64 (stateHi>>>0) (stateLo>>>0)
  in
    Seed state2 inc |> next


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
    lo'' = (lo' + c1) >>> 0
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


magicFactor = Int64 0x5851f42d 0x4c957f2d

-- derive the next seed
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
  then (peel seed0 & (max - 1), next seed0)
  else
    let
      threshhold = ((-max >>> 0) % max) >>> 0 -- essentially: big % small = small
      accountForBias : Seed -> (Int, Seed)
      accountForBias seed =
        -- in practice this recurses almost never
        let x = peel seed
            seedN = next seed
        in if x < threshhold then accountForBias seedN else (x, seedN)
    in
      accountForBias seed0

int : Int -> Int -> Seed -> (Int, Seed)
int min max =
  \seed0 ->
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

float : Float -> Float -> Seed -> (Float, Seed)
float min max =
  \seed0 ->
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


split : Seed -> (Seed, Seed)
split seed =
  --TODO: do better than this.
  (seed, next seed)
