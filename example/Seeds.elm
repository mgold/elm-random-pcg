module Seeds exposing (..)

import Time exposing (every, second)
import Html exposing (Html)
import Html.App exposing (program)
import Random.Pcg as Rand


type alias Model =
  ( Int, Rand.Seed )


type Msg
  = NewNumber


generator : Rand.Generator Int
generator =
  Rand.int 1 6


init : ( Model, Cmd Msg )
init =
  let
    result =
      Rand.step generator (Rand.initialSeed2 4240622132 2818808003)
  in
    ( result, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update NewNumber ( val, seed ) =
  let
    result =
      Rand.step generator seed
  in
    ( result, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
  every second (always NewNumber)


view : Model -> Html Msg
view ( val, _ ) =
  Html.text (toString val)


main =
  program
    { init = init, update = update, subscriptions = subscriptions, view = view }
