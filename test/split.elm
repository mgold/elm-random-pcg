module Test where

import Graphics.Element exposing (..)
import Text
import Color
import Random.PCG as Random


gen : Random.Generator (List Int)
gen = Random.list 50 (Random.int 1 6)


seeds : List Random.Seed
seeds =
  let
    f n seed =
      if n <= 0 then []
      else let (seed1, seed2) = Random.split seed
           in seed1 :: f (n-1) seed2
  in f 32 (Random.initialSeed 43)


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
        {-
        1 -> Color.red
        2 -> Color.orange
        3 -> Color.yellow
        4 -> Color.green
        5 -> Color.blue
        6 -> Color.purple
        -}
        _ -> Color.black
    style i = Text.fromString (toString i) |> Text.color (color i)
  in
    List.map style ints |> Text.concat |> leftAligned


main =
  flow down <|
    leftAligned (Text.fromString "Each list is generated from a seed split from the one before it. No obvious relationships are present in these simulated dice rolls.")
    :: List.map display xss
