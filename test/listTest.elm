module ListTest exposing (..)

{-| Rolls a die many times and collates the output. Judging whether the result is sufficiently random
is wholly unscientific; this test is really to show that independent seeds aren't *trivially* wrong.
-}

import Dict exposing (Dict)
import Random.Pcg as Random
import Html
import Html.App


seed0 : Random.Seed
seed0 =
    Random.initialSeed 628318530 |> Random.step Random.independentSeed |> snd


gen =
    -- smaller range is more likely to show problems
    Random.int 1 6
        |> Random.list 50
        |> Random.map toMultiSet


toMultiSet : List Int -> Dict Int Int
toMultiSet list =
    let
        helper xs d =
            case xs of
                [] ->
                    d

                x :: xs ->
                    helper xs <| Dict.insert x (Dict.get x d |> Maybe.withDefault 0 |> (+) 1) d
    in
        helper list Dict.empty


generated =
    Random.step gen seed0 |> fst


main : Program Never
main =
    Html.App.beginnerProgram
        { model = ()
        , update = \_ _ -> ()
        , view = \() -> Html.text <| toString <| generated
        }
