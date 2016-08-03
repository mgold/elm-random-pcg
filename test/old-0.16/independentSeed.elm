module Test where

import Graphics.Element exposing (..)
import Text
import Color
import Random.Pcg as Random


gen : Random.Generator (List Int)
gen = Random.list 50 (Random.int 1 6)


seeds : List Random.Seed
seeds =
  let
    gen = Random.list 32 Random.independentSeed
    seed = Random.initialSeed 43
  in
    Random.generate gen seed |> fst


xss : List (List Int)
xss =
  List.map (\seed -> Random.generate gen seed |> fst) seeds

display : List Int -> Element
display ints =
  let
    color i =
      case i of
        1 -> Color.lightGray
        2 -> Color.gray
        3 -> Color.darkGray
        4 -> Color.lightCharcoal
        5 -> Color.charcoal
        6 -> Color.darkCharcoal
        _ -> Color.black
    style i = Text.fromString (toString i) |> Text.color (color i)
  in
    List.map style ints |> Text.concat |> leftAligned


main =
  flow down <|
    leftAligned (Text.fromString "Each list is generated from a seed split from and independent of the one before it. No obvious relationships are present in these simulated dice rolls.")
    :: List.map display xss
