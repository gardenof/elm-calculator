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
    , valueA : Float
    , actionA : Action
    , valueB : Float
    , actionB : Action
    }


init : Model
init =
    { display = "0"
    , displayStatus = ShowingResults
    , decimalStatus = NoDecimal
    , valueA = 0
    , actionA = Blank
    , valueB = 0
    , actionB = Blank
    }


type DisplayStatus
    = ShowingResults
    | NotResults


type DecimalStatus
    = YesDecimal
    | NoDecimal


executeAdd : Model -> Model
executeAdd model =
    { model
        | display = showResults <| model.valueA + model.valueB
        , valueA = 0
        , actionA = Blank
        , valueB = 0
        , actionB = Blank
        , displayStatus = ShowingResults
    }


showResults : Float -> String
showResults float =
    let
        results =
            String.fromFloat float
    in
    if String.length results > 10 then
        "ERROR"

    else
        results



-- Update


type Msg
    = UpdateDisplay CalButton
    | Saveinput CalButton
    | AllClear


buttonToAction : CalButton -> Action
buttonToAction button =
    case button of
        CalAction action ->
            action

        _ ->
            Blank


saveAction : Model -> CalButton -> Model
saveAction model button =
    case ( model.actionA, model.actionB ) of
        ( Blank, Blank ) ->
            { model
                | valueA = displatToFloat model.display
                , actionA = buttonToAction button
                , displayStatus = ShowingResults
            }

        ( _, _ ) ->
            executeAdd
                { model
                    | valueB = displatToFloat model.display
                    , actionB = buttonToAction button
                    , displayStatus = ShowingResults
                }


displatToFloat : String -> Float
displatToFloat string =
    case String.toFloat string of
        Just float ->
            float

        Nothing ->
            0


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
        AllClear ->
            init

        UpdateDisplay buttonClicked ->
            updateDisplay model buttonClicked

        Saveinput buttonClicked ->
            saveAction model buttonClicked



-- View


view : Model -> Html Msg
view model =
    div [ class "calculator" ]
        [ div [] [ Html.node "style" [] [ text Css.css ] ]
        , div [ class "display" ] [ text model.display ]
        , div [ class "buttons" ]
            [ actionButton (CalAction Add)
            , button [ class "operator" ] [ text "-" ]
            , button [ class "operator" ] [ text "/" ]
            , button [ class "operator" ] [ text "*" ]
            , numberButton CalNumOne
            , numberButton CalNumTwo
            , numberButton CalNumThree
            , numberButton CalNumFour
            , numberButton CalNumFive
            , numberButton CalNumSix
            , numberButton CalNumSeven
            , numberButton CalNumEight
            , numberButton CalNumNine
            , numberButton CalNumZero
            , button [ class "allClear", onClick AllClear ] [ text "AC" ]
            , numberButton CalDecimal
            , equalButton (CalAction Equals)
            ]
        , devHtml model
        ]


numberButton : CalButton -> Html Msg
numberButton button =
    Html.button
        [ class "number"
        , onClick (UpdateDisplay button)
        ]
        [ text <| buttonValue button ]


actionButton : CalButton -> Html Msg
actionButton button =
    Html.button
        [ class "operator"
        , onClick (Saveinput button)
        ]
        [ text <| buttonValue button ]


equalButton : CalButton -> Html Msg
equalButton button =
    Html.button
        [ class "equal"
        , onClick (Saveinput button)
        ]
        [ text <| buttonValue button ]



--Dev
-- On a normal project would not commit below code


devHtml : Model -> Html Msg
devHtml model =
    Html.div [ class "dev" ]
        [ div [] [ text "display : ", text model.display ]
        , div [] [ text "displayStatus : ", text <| displayStatusToString model.displayStatus ]
        , div [] [ text "decimalStatus : ", text <| decimalStatusToString model.decimalStatus ]
        , div [] [ text "value A : ", text <| String.fromFloat model.valueA ]
        , div [] [ text "actionA : ", text <| actionToString model.actionA ]
        , div [] [ text "value B : ", text <| String.fromFloat model.valueB ]
        , div [] [ text "actionB : ", text <| actionToString model.actionB ]
        ]


displayStatusToString : DisplayStatus -> String
displayStatusToString displayStatus =
    case displayStatus of
        ShowingResults ->
            "ShowingResults"

        NotResults ->
            "NotResults"


decimalStatusToString : DecimalStatus -> String
decimalStatusToString status =
    case status of
        YesDecimal ->
            "YesDecimal"

        NoDecimal ->
            "NoDecimal"


actionToString : Action -> String
actionToString action =
    case action of
        Blank ->
            "Blank"

        Add ->
            "Add"

        Equals ->
            "Equals"
