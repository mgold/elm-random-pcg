module Test where


import Graphics.Collage exposing (rect, move, collage, filled, group)
import Color
import Multiset exposing (Multiset)
import Random--.PCG as Random

top = 1800
n = top*40

gen : Random.Generator (List Int)
gen = Random.list n (Random.int 0 top)

seed : Random.Seed
seed = Random.initialSeed 47

xs : Multiset Int
xs = Random.generate gen seed |> fst |> Multiset.fromList

frame =
  let
    (w,h) = (top, 400)
    largest = Multiset.foldl (\i c a -> max c a) 0 xs
    width = 1
    bar i =
      let
        count = Multiset.get i xs
        height = toFloat <| h * count//largest
        offset = toFloat i
      in
        rect width height |> filled Color.blue |> move (offset, height/2)
  in
    collage w h [
      group (List.map bar [0..top])
        |> move (-w/2 + width/2, toFloat (-h//2))]


main = frame
