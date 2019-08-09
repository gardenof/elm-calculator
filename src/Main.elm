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
    , displayStatus = ShowingResults
    , decimalStatus = NoDecimal
    }



-- Update


type Msg
    = UpdateDisplay CalButton


type DisplayStatus
    = ShowingResults
    | NotResults


type DecimalStatus
    = YesDecimal
    | NoDecimal


updateDisplay : Model -> CalButton -> Model
updateDisplay model buttonClicked =
    determineUpdateDisplay model
        buttonClicked
        ( model.displayStatus
        , buttonType buttonClicked
        , model.decimalStatus
        )


determineUpdateDisplay : Model -> CalButton -> ( DisplayStatus, ButtonType, DecimalStatus ) -> Model
determineUpdateDisplay model buttonClicked tuple =
    case tuple of
        ( NotResults, Decimal, NoDecimal ) ->
            { model
                | display = model.display ++ buttonValue buttonClicked
                , decimalStatus = YesDecimal
            }

        ( NotResults, Decimal, YesDecimal ) ->
            model

        ( NotResults, Num, _ ) ->
            { model | display = model.display ++ buttonValue buttonClicked }

        ( ShowingResults, Decimal, _ ) ->
            { model
                | display = buttonValue CalNumZero ++ buttonValue buttonClicked
                , decimalStatus = YesDecimal
                , displayStatus = NotResults
            }

        ( ShowingResults, Num, _ ) ->
            { model | display = buttonValue buttonClicked, displayStatus = NotResults }


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
            , button
                [ class "number"
                , onClick (UpdateDisplay CalNumOne)
                ]
                [ text <| buttonValue CalNumOne ]
            , button
                [ class "number"
                , onClick (UpdateDisplay CalNumTwo)
                ]
                [ text <| buttonValue CalNumTwo ]
            , button
                [ class "number"
                , onClick (UpdateDisplay CalNumThree)
                ]
                [ text <| buttonValue CalNumThree ]
            , button
                [ class "number"
                , onClick (UpdateDisplay CalNumFour)
                ]
                [ text <| buttonValue CalNumFour ]
            , button
                [ class "number"
                , onClick (UpdateDisplay CalNumFive)
                ]
                [ text <| buttonValue CalNumFive ]
            , button
                [ class "number"
                , onClick (UpdateDisplay CalNumSix)
                ]
                [ text <| buttonValue CalNumSix ]
            , button
                [ class "number"
                , onClick (UpdateDisplay CalNumSeven)
                ]
                [ text <| buttonValue CalNumSeven ]
            , button
                [ class "number"
                , onClick (UpdateDisplay CalNumEight)
                ]
                [ text <| buttonValue CalNumEight ]
            , button
                [ class "number"
                , onClick (UpdateDisplay CalNumNine)
                ]
                [ text <| buttonValue CalNumNine ]
            , button
                [ class "number"
                , onClick (UpdateDisplay CalNumZero)
                ]
                [ text <| buttonValue CalNumZero ]
            , button [ class "clearAll" ] [ text "AC" ]
            , button
                [ class "decimal"
                , onClick <| UpdateDisplay CalDecimal
                ]
                [ text <| buttonValue CalDecimal ]
            , button [ class "equal" ] [ text "=" ]
            ]
        ]
