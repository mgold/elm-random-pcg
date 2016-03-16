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
import Random.Pcg as Pcg


coreSeed : Ran.Seed
coreSeed =
  Ran.initialSeed 141053960


pcgSeed : Pcg.Seed
pcgSeed =
  Pcg.initialSeed2 141053960 490238886


n =
  1000


main =
  Signal.map (Graphics.Element.leftAligned << Text.fromString) results.signal


mySuite =
  Benchmark.Suite
    "Decoding suite"
    [ Benchmark.bench2 "Core: flip a coin" Ran.generate Ran.bool coreSeed
    , Benchmark.bench2 "Pcg:  flip a coin" Pcg.generate Pcg.bool pcgSeed
    , Benchmark.bench2 ("Core: flip " ++ toString n ++ " coins") Ran.generate (Ran.list n Ran.bool) coreSeed
    , Benchmark.bench2 ("Pcg:  flip " ++ toString n ++ " coins") Pcg.generate (Pcg.list n Pcg.bool) pcgSeed
    , Benchmark.bench2 "Core: generate an integer 0-4094" Ran.generate (Ran.int 0 4094) coreSeed
    , Benchmark.bench2 "Pcg:  generate an integer 0-4094" Pcg.generate (Pcg.int 0 4094) pcgSeed
    , Benchmark.bench2 "Core: generate an integer 0-4095" Ran.generate (Ran.int 0 4095) coreSeed
    , Benchmark.bench2 "Pcg:  generate an integer 0-4095" Pcg.generate (Pcg.int 0 4095) pcgSeed
    , Benchmark.bench2 "Core: generate an integer 0-4096" Ran.generate (Ran.int 0 4096) coreSeed
    , Benchmark.bench2 "Pcg:  generate an integer 0-4096" Pcg.generate (Pcg.int 0 4096) pcgSeed
    , Benchmark.bench2 "Core: generate a massive integer" Ran.generate (Ran.int 0 4294967295) coreSeed
    , Benchmark.bench2 "Pcg:  generate a massive integer" Pcg.generate (Pcg.int 0 4294967295) pcgSeed
    , Benchmark.bench2 "Core: generate a percentage" Ran.generate (Ran.float 0 1) coreSeed
    , Benchmark.bench2 "Pcg:  generate a percentage" Pcg.generate (Pcg.float 0 1) pcgSeed
    , Benchmark.bench2 ("Core: generate " ++ toString n ++ " percentages") Ran.generate (Ran.list n (Ran.float 0 1)) coreSeed
    , Benchmark.bench2 ("Pcg:  generate " ++ toString n ++ " percentages") Pcg.generate (Pcg.list n (Pcg.float 0 1)) pcgSeed
    , Benchmark.bench2 "Core: generate a float 0-4094" Ran.generate (Ran.float 0 4094) coreSeed
    , Benchmark.bench2 "Pcg:  generate a float 0-4094" Pcg.generate (Pcg.float 0 4094) pcgSeed
    , Benchmark.bench2 "Core: generate a float 0-4095" Ran.generate (Ran.float 0 4095) coreSeed
    , Benchmark.bench2 "Pcg:  generate a float 0-4095" Pcg.generate (Pcg.float 0 4095) pcgSeed
    , Benchmark.bench2 "Core: generate a float 0-4096" Ran.generate (Ran.float 0 4096) coreSeed
    , Benchmark.bench2 "Pcg:  generate a float 0-4096" Pcg.generate (Pcg.float 0 4096) pcgSeed
    , Benchmark.bench2 "Core: generate a massive float" Ran.generate (Ran.float 0 4294967295) coreSeed
    , Benchmark.bench2 "Pcg:  generate a massive float" Pcg.generate (Pcg.float 0 4294967295) pcgSeed
    , Benchmark.bench2 "Pcg:  generate an independent (split) seed" Pcg.generate (Pcg.independentSeed) pcgSeed
    , Benchmark.bench2 "Pcg:  fast forward a seed 1 time" Pcg.fastForward 1 pcgSeed
    , Benchmark.bench2 "Pcg:  fast forward a seed 10 times" Pcg.fastForward 10 pcgSeed
    , Benchmark.bench2 "Pcg:  fast forward a seed 100 times" Pcg.fastForward 100 pcgSeed
    , Benchmark.bench2 "Pcg:  fast forward a seed 1000 times" Pcg.fastForward 1000 pcgSeed
    , Benchmark.bench2 "Pcg:  fast forward a seed 1 million times" Pcg.fastForward 1000000 pcgSeed
    ]


results : Signal.Mailbox String
results =
  Signal.mailbox "Benchmark loading"


port benchResults : Task Benchmark.Never ()
port benchResults =
  Benchmark.runWithProgress (Just results) mySuite
    `Task.andThen` (\_ -> Task.succeed ())
