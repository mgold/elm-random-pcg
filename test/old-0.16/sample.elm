module Test (..) where

{-| Test the sample function.
-}

import Graphics.Element exposing (..)
import Random.Pcg as Random exposing (Generator, list, sample, generate, initialSeed2)


seed0 =
  initialSeed2 734080189 1239598079


genList =
  list 12 <| sample [ True, False ]


( generated, _ ) =
  generate genList seed0
main =
  show generated
