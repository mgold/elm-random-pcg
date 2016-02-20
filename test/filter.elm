module Test where
{-| Test the filter function. The list displayed should contain only even integers.
-}

import Graphics.Element exposing (..)
import Random.PCG as Random exposing (Generator, list, filter, int, minInt, maxInt, generate, initialSeed)


gen : Generator (List Int)
gen =
  list 20 <| filter (\i -> i % 2 == 0) (int minInt maxInt)

generated =
  generate gen (initialSeed 42) |> fst


main =
  show generated
