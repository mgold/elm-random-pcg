module Test where


import Graphics.Element exposing (show)
import Graphics.Collage exposing (rect, move, collage, filled, group)
import Color
import Multiset exposing (Multiset)
import Random.PCGelm as Random

top = 1800
n = top*400

gen : Random.Generator (List Int)
gen = Random.list n (Random.int 0 top)

seed : Random.Seed
seed = Random.initialSeed 47

xs : Multiset Int
xs = Random.generate gen seed |> fst |> Multiset.fromList

main = show "DONE!!!!!!!!!!!!!!!!"
