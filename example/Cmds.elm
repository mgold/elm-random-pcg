module Cmds exposing (..)

import Time exposing (every, second)
import Html exposing (Html)
import Html.App exposing (program)
import Random.Pcg as Rand


type alias Model =
  Int


type Msg
  = RequestANewNumber
  | HereIsANewNumber Int


generator : Rand.Generator Int
generator =
  Rand.int 1 6


init : ( Model, Cmd Msg )
init =
  -- there's no way to generate an initial random value with commands
  -- although the first command is run so fast you can't really notice it.
  ( 1, Rand.generate HereIsANewNumber generator )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg val =
  case msg of
    RequestANewNumber ->
      ( val, Rand.generate HereIsANewNumber generator )

    HereIsANewNumber newVal ->
      ( newVal, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
  every second (always RequestANewNumber)


view : Model -> Html Msg
view val =
  Html.text (toString val)


main =
  program
    { init = init, update = update, subscriptions = subscriptions, view = view }
