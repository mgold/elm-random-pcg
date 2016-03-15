{- JSON parsing stess test. Download the data file:
wget http://www.eecs.tufts.edu/~mgolds07/packet_viz/multiview/data/data.0-239.json
-}


module Main (..) where

import Graphics.Element exposing (show)
import Signal
import Task exposing (Task)
import String
import Text
import Benchmark
import Random as Ran
import Random.PCG as PCG


coreSeed : Ran.Seed
coreSeed =
  Ran.initialSeed 141053960


pcgSeed : PCG.Seed
pcgSeed =
  PCG.initialSeed2 141053960 490238886


n =
  1000


main =
  Signal.map (Graphics.Element.leftAligned << Text.fromString) results.signal


mySuite =
  Benchmark.Suite
    "Decoding suite"
    [ Benchmark.bench2 "Core: flip a coin" Ran.generate Ran.bool coreSeed
    , Benchmark.bench2 "PCG:  flip a coin" PCG.generate PCG.bool pcgSeed
    , Benchmark.bench2 ("Core: flip " ++ toString n ++ " coins") Ran.generate (Ran.list n Ran.bool) coreSeed
    , Benchmark.bench2 ("PCG:  flip " ++ toString n ++ " coins") PCG.generate (PCG.list n PCG.bool) pcgSeed
    , Benchmark.bench2 "Core: generate an integer 0-4094" Ran.generate (Ran.int 0 4094) coreSeed
    , Benchmark.bench2 "PCG:  generate an integer 0-4094" PCG.generate (PCG.int 0 4094) pcgSeed
    , Benchmark.bench2 "Core: generate an integer 0-4095" Ran.generate (Ran.int 0 4095) coreSeed
    , Benchmark.bench2 "PCG:  generate an integer 0-4095" PCG.generate (PCG.int 0 4095) pcgSeed
    , Benchmark.bench2 "Core: generate an integer 0-4096" Ran.generate (Ran.int 0 4096) coreSeed
    , Benchmark.bench2 "PCG:  generate an integer 0-4096" PCG.generate (PCG.int 0 4096) pcgSeed
    , Benchmark.bench2 "Core: generate a massive integer" Ran.generate (Ran.int 0 4294967295) coreSeed
    , Benchmark.bench2 "PCG:  generate a massive integer" PCG.generate (PCG.int 0 4294967295) pcgSeed
    , Benchmark.bench2 "Core: generate a percentage" Ran.generate (Ran.float 0 1) coreSeed
    , Benchmark.bench2 "PCG:  generate a percentage" PCG.generate (PCG.float 0 1) pcgSeed
    , Benchmark.bench2 ("Core: generate " ++ toString n ++ " percentages") Ran.generate (Ran.list n (Ran.float 0 1)) coreSeed
    , Benchmark.bench2 ("PCG:  generate " ++ toString n ++ " percentages") PCG.generate (PCG.list n (PCG.float 0 1)) pcgSeed
    , Benchmark.bench2 "Core: generate a float 0-4094" Ran.generate (Ran.float 0 4094) coreSeed
    , Benchmark.bench2 "PCG:  generate a float 0-4094" PCG.generate (PCG.float 0 4094) pcgSeed
    , Benchmark.bench2 "Core: generate a float 0-4095" Ran.generate (Ran.float 0 4095) coreSeed
    , Benchmark.bench2 "PCG:  generate a float 0-4095" PCG.generate (PCG.float 0 4095) pcgSeed
    , Benchmark.bench2 "Core: generate a float 0-4096" Ran.generate (Ran.float 0 4096) coreSeed
    , Benchmark.bench2 "PCG:  generate a float 0-4096" PCG.generate (PCG.float 0 4096) pcgSeed
    , Benchmark.bench2 "Core: generate a massive float" Ran.generate (Ran.float 0 4294967295) coreSeed
    , Benchmark.bench2 "PCG:  generate a massive float" PCG.generate (PCG.float 0 4294967295) pcgSeed
    , Benchmark.bench1 "PCG:  split a seed" PCG.split pcgSeed
    , Benchmark.bench2 "PCG:  fast forward a seed 1 time" PCG.fastForward 1 pcgSeed
    , Benchmark.bench2 "PCG:  fast forward a seed 10 times" PCG.fastForward 10 pcgSeed
    , Benchmark.bench2 "PCG:  fast forward a seed 100 times" PCG.fastForward 100 pcgSeed
    , Benchmark.bench2 "PCG:  fast forward a seed 1000 times" PCG.fastForward 1000 pcgSeed
    , Benchmark.bench2 "PCG:  fast forward a seed 1 million times" PCG.fastForward 1000000 pcgSeed
    ]


results : Signal.Mailbox String
results =
  Signal.mailbox "Benchmark loading"


port benchResults : Task Benchmark.Never ()
port benchResults =
  Benchmark.runWithProgress (Just results) mySuite
    `Task.andThen` (\_ -> Task.succeed ())
