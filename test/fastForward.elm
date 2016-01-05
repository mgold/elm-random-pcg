module FastForwardTest where

{-| Compares the outputs of seeds created using the ordinary sequential stepping method,
and those created using the `fastForward` function. Note that we compare output, not the
seeds themselves, because somestimes the states get confused between signed and unsigned--
this has no effect on the psuedo-random output.
-}

import Graphics.Element exposing (show)
import Random.PCG as Random

n = 100000

seed0 : Random.Seed
seed0 = Random.initialSeed2 1618 628318530

stepped : List Random.Seed
stepped =
  List.scanl
    (\_ oldSeed -> Random.generate Random.bool oldSeed |> snd)
    seed0
    [1..n]

fastForwarded : List Random.Seed
fastForwarded =
  List.map
    (\i -> Random.fastForward i seed0)
    [0..n]

gen = Random.int 1 10000

generate seed =
  Random.generate gen seed |> fst

bools =
  List.map2
    (\seed1 seed2 -> generate seed1 == generate seed2)
    stepped
    fastForwarded

main =
  show <| List.all identity bools
