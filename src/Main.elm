module Main exposing (main)

import Browser
import Css
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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
    div [ class "calculator" ]
        [ div [] [ Html.node "style" [] [ text Css.css ] ]
        , div [ class "display" ] [ text "display" ]
        , div [ class "buttons" ]
            [ button [ class "operator" ] [ text "+" ]
            , button [ class "operator" ] [ text "-" ]
            , button [ class "operator" ] [ text "/" ]
            , button [ class "operator" ] [ text "*" ]
            , button [ class "number" ] [ text "1" ]
            , button [ class "number" ] [ text "2" ]
            , button [ class "number" ] [ text "3" ]
            , button [ class "number" ] [ text "4" ]
            , button [ class "number" ] [ text "5" ]
            , button [ class "number" ] [ text "6" ]
            , button [ class "number" ] [ text "7" ]
            , button [ class "number" ] [ text "8" ]
            , button [ class "number" ] [ text "9" ]
            , button [ class "number" ] [ text "0" ]
            , button [ class "clearAll" ] [ text "AC" ]
            , button [ class "decimal" ] [ text "." ]
            , button [ class "equal" ] [ text "=" ]
            ]
        ]
