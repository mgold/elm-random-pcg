module Test where

import Graphics.Element exposing (..)
import Random.PCG as Random


gen : Random.Generator (List Int)
gen = Random.list 20 (Random.int 1 6)


seeds : List Random.Seed
seeds =
  let
    f n seed =
      if n <= 0 then []
      else let (seed1, seed2) = Random.split seed
           in seed1 :: f (n-1) seed2
  in f 20 (Random.initialSeed 43)


xss : List (List Int)
xss =
  List.map (\seed -> Random.generate gen seed |> fst) seeds


main =
  flow down (List.map show xss)
