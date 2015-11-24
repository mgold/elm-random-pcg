module Test where

import Graphics.Element exposing (..)
import Random.PCG as Random

gen : Random.Generator Int
gen = Random.int 0 32

seed : Random.Seed
seed = Random.initialSeed 42

main =
  flow down <|
    List.map (\_ -> Random.generate gen seed |> fst |> show) [0..4]
