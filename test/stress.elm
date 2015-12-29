module Test where

import Graphics.Element exposing (show)
import Random.PCG as Random

top = 1800
n = top*400

gen : Random.Generator (List Int)
gen = Random.list n (Random.int 0 top)

seed : Random.Seed
seed = Random.initialSeed 47

test = Random.generate gen seed

main = show "DONE!!!!!!!!!!!!!!!!"
