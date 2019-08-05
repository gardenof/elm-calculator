module Main exposing (main)

import Browser
import Html exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type alias Model =
    {}


init : Model
init =
    {}



-- Update


type Msg
    = Message


update : Msg -> Model -> Model
update msg model =
    model



-- View


view : Model -> Html Msg
view model =
    text "Hello World"
