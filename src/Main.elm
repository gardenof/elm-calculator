module Main exposing (main)

import Browser
import CalculatorNumbers exposing (..)
import Css
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type alias Model =
    { display : String }


init : Model
init =
    { display = "0" }



-- Update


type Msg
    = UpdateDisplay String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateDisplay updatedNumber ->
            { model | display = updatedNumber }



-- View


view : Model -> Html Msg
view model =
    div [ class "calculator" ]
        [ div [] [ Html.node "style" [] [ text Css.css ] ]
        , div [ class "display" ] [ text model.display ]
        , div [ class "buttons" ]
            [ button [ class "operator" ] [ text "+" ]
            , button [ class "operator" ] [ text "-" ]
            , button [ class "operator" ] [ text "/" ]
            , button [ class "operator" ] [ text "*" ]
            , button [ class "number", onClick (UpdateDisplay calNumOne) ] [ text calNumOne ]
            , button [ class "number", onClick (UpdateDisplay calNumTwo) ] [ text calNumTwo ]
            , button [ class "number", onClick (UpdateDisplay calNumThree) ] [ text calNumThree ]
            , button [ class "number", onClick (UpdateDisplay calNumFour) ] [ text calNumFour ]
            , button [ class "number", onClick (UpdateDisplay calNumFive) ] [ text calNumFive ]
            , button [ class "number", onClick (UpdateDisplay calNumSix) ] [ text calNumSix ]
            , button [ class "number", onClick (UpdateDisplay calNumSeven) ] [ text calNumSeven ]
            , button [ class "number", onClick (UpdateDisplay calNumEight) ] [ text calNumEight ]
            , button [ class "number", onClick (UpdateDisplay calNumNine) ] [ text calNumNine ]
            , button [ class "number", onClick (UpdateDisplay calNumZero) ] [ text calNumZero ]
            , button [ class "clearAll" ] [ text "AC" ]
            , button [ class "decimal" ] [ text "." ]
            , button [ class "equal" ] [ text "=" ]
            ]
        ]
