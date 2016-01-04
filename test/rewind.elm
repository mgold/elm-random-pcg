module RewindTest where

{-| Demonstrates that `fastForward` may also be used to rewind. We create a seed, fastForward it, then rewind it to get
back the original seed.
-}

import Graphics.Element exposing (..)
import Random.PCG as Random

n = 57

seed0 = Random.initialSeed2 1618 628318

seed1 = Random.fastForward  n seed0
seed2 = Random.fastForward -n seed1


main =
  flow down <| List.map show [seed0, seed1, seed2]
