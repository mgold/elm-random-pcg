module Test (..) where

{-| Test the oneIn function. Ensure the the actual probability matches the declared probability. Try changing n.
-}

import Graphics.Element exposing (..)
import Random.Pcg as Random exposing (Generator, list, oneIn, generate, initialSeed2)


n =
  4


gen : Generator (List Bool)
gen =
  list 100000 <| oneIn n


generated =
  generate gen (initialSeed2 734080189 1239598079) |> fst


( true, false ) =
  List.partition identity generated
trueCount =
  toFloat <| List.length true


falseCount =
  toFloat <| List.length false


main =
  show <| "Expected 1 to " ++ toString n ++ ", got: " ++ toString (trueCount / (trueCount + falseCount))
