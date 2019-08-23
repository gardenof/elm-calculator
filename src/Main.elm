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


type ActionType
    = AddSubtractType
    | MultiplyDivideType
    | EqualsType



-- Update


type Msg
    = UpdateDisplay CalButton
    | Saveinput Action
    | AllClear


saveAction : Model -> Action -> Model
saveAction model newAction =
    case ( model.actionA, model.actionB ) of
        ( Just Equals, Nothing ) ->
            saveActionA model newAction

        ( Nothing, Nothing ) ->
            saveActionA model newAction

        ( Just a, Nothing ) ->
            checkActionAandNewAction model a newAction

        ( Just a, Just b ) ->
            checkActionBandNewAction model a b newAction

        ( Nothing, Just b ) ->
            model


saveActionA : Model -> Action -> Model
saveActionA model action =
    if action == Equals then
        model

    else
        { model
            | valueA = displatToFloat model.display
            , actionA = Just action
            , displayStatus = ShowingResults
        }


actionTypeOf : Action -> ActionType
actionTypeOf action =
    case action of
        Add ->
            AddSubtractType

        Subtract ->
            AddSubtractType

        Multiply ->
            MultiplyDivideType

        Divide ->
            MultiplyDivideType

        Equals ->
            EqualsType


checkActionAandNewAction : Model -> Action -> Action -> Model
checkActionAandNewAction model actionA newAction =
    if
        (actionTypeOf actionA == actionTypeOf newAction)
            || (newAction == Equals)
    then
        doMathOnAandNewAction model actionA newAction

    else
        saveActionB model newAction


saveActionB : Model -> Action -> Model
saveActionB model action =
    { model
        | displayStatus = ShowingResults
        , valueB = displatToFloat model.display
        , actionB = Just action
    }


doMathOnAandNewAction : Model -> Action -> Action -> Model
doMathOnAandNewAction model actionA newAction =
    let
        results =
            simpleMath
                model.valueA
                actionA
                (displatToFloat model.display)
    in
    { model
        | display = showResults <| results
        , displayStatus = ShowingResults
        , valueA = results
        , actionA = Just newAction
        , valueB = 0
        , actionB = Nothing
    }


simpleMath : Float -> Action -> Float -> Float
simpleMath numOne action numTwo =
    case action of
        Add ->
            numOne + numTwo

        Subtract ->
            numOne - numTwo

        Multiply ->
            numOne * numTwo

        Divide ->
            numOne / numTwo

        Equals ->
            numOne


checkActionBandNewAction : Model -> Action -> Action -> Action -> Model
checkActionBandNewAction model actionA actionB newAction =
    if
        (actionTypeOf actionB == actionTypeOf newAction)
            || ((actionTypeOf actionA == AddSubtractType)
                    && (actionTypeOf actionB == MultiplyDivideType)
               )
    then
        doMathOnBandNewAction model actionA actionB newAction

    else
        doMathOnAandB model actionA actionB newAction


doMathOnBandNewAction : Model -> Action -> Action -> Action -> Model
doMathOnBandNewAction model actionA actionB newAction =
    let
        results =
            simpleMath
                model.valueB
                actionB
                (displatToFloat model.display)
    in
    checkActionAandNewAction
        { model
            | display = showResults <| results
            , displayStatus = ShowingResults
            , valueB = 0
            , actionB = Nothing
        }
        actionA
        newAction


doMathOnAandB : Model -> Action -> Action -> Action -> Model
doMathOnAandB model actionA actionB newAction =
    checkActionAandNewAction
        { model
            | valueA = simpleMath model.valueA actionA model.valueB
            , actionA = Just actionB
            , valueB = 0
            , actionB = Nothing
        }
        actionB
        newAction


showResults : Float -> String
showResults float =
    let
        results =
            String.fromFloat float

        hasDecimal =
            String.any (\a -> '.' == a) results

        largerThanTen =
            if hasDecimal then
                String.length results > 11

            else
                String.length results > 10
    in
    if largerThanTen then
        "ERROR"

    else
        results


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
        , div [] [ text "actionA : ", text <| maybeActionToString model.actionA ]
        , div [] [ text "value B : ", text <| String.fromFloat model.valueB ]
        , div [] [ text "actionB : ", text <| maybeActionToString model.actionB ]
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


maybeActionToString : Maybe Action -> String
maybeActionToString action =
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
