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
    , actionA : Maybe Action
    , valueB : Float
    , actionB : Maybe Action
    }


init : Model
init =
    { display = "0"
    , displayStatus = ShowingResults
    , decimalStatus = NoDecimal
    , valueA = 0
    , actionA = Nothing
    , valueB = 0
    , actionB = Nothing
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
        , actionA = Nothing
        , valueB = 0
        , actionB = Nothing
        , displayStatus = ShowingResults
    }


executeSubtract : Model -> Model
executeSubtract model =
    { model
        | display = String.fromFloat <| model.valueA - model.valueB
        , valueA = 0
        , actionA = Nothing
        , valueB = 0
        , actionB = Nothing
        , displayStatus = ShowingResults
    }


executeMultiply : Model -> Model
executeMultiply model =
    { model
        | display = String.fromFloat <| model.valueA * model.valueB
        , valueA = 0
        , actionA = Nothing
        , valueB = 0
        , actionB = Nothing
        , displayStatus = ShowingResults
    }


executeDivide : Model -> Model
executeDivide model =
    { model
        | display = String.fromFloat <| model.valueA / model.valueB
        , valueA = 0
        , actionA = Nothing
        , valueB = 0
        , actionB = Nothing
        , displayStatus = ShowingResults
    }


showResults : Float -> String
showResults float =
    let
        string =
            String.fromFloat float

        hasDecimal =
            String.any (\a -> '.' == a) string

        largerThanTen =
            if hasDecimal then
                String.length string > 11

            else
                String.length string > 10
    in
    if largerThanTen then
        "ERROR"

    else
        string



-- Update


type Msg
    = UpdateDisplay CalButton
    | Saveinput Action
    | AllClear


savedValueBs : Model -> Action -> Model
savedValueBs model action =
    { model
        | valueB = displatToFloat model.display
        , actionB = Just action
        , displayStatus = ShowingResults
    }


saveAction : Model -> Action -> Model
saveAction model action =
    case model.actionA of
        Nothing ->
            { model
                | valueA = displatToFloat model.display
                , actionA = Just action
                , displayStatus = ShowingResults
            }

        Just Add ->
            executeAdd <| savedValueBs model action

        Just Subtract ->
            executeSubtract <| savedValueBs model action

        Just Multiply ->
            executeMultiply <| savedValueBs model action

        Just Divide ->
            executeDivide <| savedValueBs model action

        Just Equals ->
            model


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

        Saveinput action ->
            saveAction model action



-- View


view : Model -> Html Msg
view model =
    div [ class "calculator" ]
        [ div [] [ Html.node "style" [] [ text Css.css ] ]
        , div [ class "display" ] [ text model.display ]
        , div [ class "buttons" ]
            [ actionButton Add
            , actionButton Subtract
            , actionButton Divide
            , actionButton Multiply
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
            , equalButton Equals
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


actionButton : Action -> Html Msg
actionButton action =
    Html.button
        [ class "operator"
        , onClick (Saveinput action)
        ]
        [ text <| actionSymbolToString action ]


equalButton : Action -> Html Msg
equalButton action =
    Html.button
        [ class "equal"
        , onClick (Saveinput action)
        ]
        [ text <| actionSymbolToString action ]



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


actionToString : Maybe Action -> String
actionToString action =
    case action of
        Just Add ->
            "Add"

        Just Equals ->
            "Equals"

        Just Subtract ->
            "Subtract"

        Just Multiply ->
            "Multiply"

        Just Divide ->
            "Divide"

        Nothing ->
            "Nothing"
