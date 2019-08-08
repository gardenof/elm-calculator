module Main exposing (main)

import Browser
import CalculatorButtonValues exposing (..)
import Css
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type alias Model =
    { display : String
    , displayStatus : DisplayStatus
    , decimalStatus : DecimalStatus
    }


init : Model
init =
    { display = "0"
    , displayStatus = Locked
    , decimalStatus = NoDecimal
    }



-- Update


type Msg
    = UpdateDisplay String


type InputType
    = Num
    | Decimal


type DisplayStatus
    = Locked
    | Unlocked


type DecimalStatus
    = YesDecimal
    | NoDecimal


inputType : String -> InputType
inputType string =
    if String.contains "." string then
        Decimal

    else
        Num


updateDisplay : Model -> String -> Model
updateDisplay model buttonClicked =
    determineUpdateDisplay model
        buttonClicked
        ( model.displayStatus
        , inputType buttonClicked
        , model.decimalStatus
        )


determineUpdateDisplay : Model -> String -> ( DisplayStatus, InputType, DecimalStatus ) -> Model
determineUpdateDisplay model buttonClicked tuple =
    case tuple of
        ( Unlocked, Decimal, NoDecimal ) ->
            { model
                | display = model.display ++ buttonClicked
                , decimalStatus = YesDecimal
            }

        ( Unlocked, Decimal, YesDecimal ) ->
            model

        ( Unlocked, Num, _ ) ->
            { model | display = model.display ++ buttonClicked }

        ( Locked, Decimal, _ ) ->
            { model
                | display = calNumZero ++ buttonClicked
                , decimalStatus = YesDecimal
                , displayStatus = Unlocked
            }

        ( Locked, Num, _ ) ->
            { model | display = buttonClicked, displayStatus = Unlocked }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateDisplay buttonClicked ->
            updateDisplay model buttonClicked



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
            , button [ class "decimal", onClick (UpdateDisplay calDecimal) ] [ text calDecimal ]
            , button [ class "equal" ] [ text "=" ]
            ]
        ]
