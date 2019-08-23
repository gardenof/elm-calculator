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
    , dismantledNum : NumberParts
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
    , dismantledNum = { total = 0, beforeDecimal = 0, afterDecimal = 0 }
    }


type DisplayStatus
    = ShowingResults
    | NotResults


type DecimalStatus
    = NoDecimal
    | ClickedDecimal
    | HasDecimal


type ActionType
    = AddSubtractType
    | MultiplyDivideType
    | EqualsType


type alias NumberParts =
    { total : Float
    , beforeDecimal : Float
    , afterDecimal : Float
    }


concatNumbers : Model -> Float -> Model
concatNumbers model num2 =
    let
        dismantledNumShortcut =
            model.dismantledNum
    in
    case model.decimalStatus of
        HasDecimal ->
            let
                newNum =
                    (dismantledNumShortcut.afterDecimal * 10) + num2

                newDismantledNum =
                    buildTotal
                        { dismantledNumShortcut | afterDecimal = newNum }
            in
            { model
                | display = String.fromFloat model.dismantledNum.beforeDecimal ++ "." ++ String.fromFloat newNum
                , dismantledNum = newDismantledNum
            }

        ClickedDecimal ->
            { model
                | display = String.fromFloat model.dismantledNum.beforeDecimal ++ "."
                , decimalStatus = HasDecimal
            }

        NoDecimal ->
            let
                newNum =
                    (dismantledNumShortcut.beforeDecimal * 10) + num2

                newDismantledNum =
                    buildTotal
                        { dismantledNumShortcut | beforeDecimal = newNum }
            in
            { model
                | display = String.fromFloat newNum
                , dismantledNum = newDismantledNum
            }


buildTotal : NumberParts -> NumberParts
buildTotal numberModel =
    { numberModel
        | total =
            numberModel.beforeDecimal
                + divideByPowerOfLength numberModel.afterDecimal
    }


divideByPowerOfLength : Float -> Float
divideByPowerOfLength afterDecimal =
    afterDecimal / toFloat (10 ^ lengthOfFloat afterDecimal)


lengthOfFloat : Float -> Int
lengthOfFloat float =
    String.length <| String.fromFloat float



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
            | valueA = model.dismantledNum.total
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
        , valueB = model.dismantledNum.total
        , actionB = Just action
    }


doMathOnAandNewAction : Model -> Action -> Action -> Model
doMathOnAandNewAction model actionA newAction =
    let
        newNum =
            model.dismantledNum

        results =
            simpleMath
                model.valueA
                actionA
                newNum.total

        newDismantledNum =
            { newNum | total = results }
    in
    { model
        | display = showResults newDismantledNum.total
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
        dismantledNumShortcut =
            model.dismantledNum

        results =
            simpleMath
                model.valueB
                actionB
                dismantledNumShortcut.total

        newDismantledNum =
            { dismantledNumShortcut | total = results }

        newModel =
            { model
                | display = showResults newDismantledNum.total
                , displayStatus = ShowingResults
                , valueB = 0
                , actionB = Nothing
                , dismantledNum = newDismantledNum
            }
    in
    checkActionAandNewAction
        newModel
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
    let
        dismantledNumShortcut =
            model.dismantledNum
    in
    case tuple of
        ( ShowingResults, Num, _ ) ->
            let
                newNum =
                    buttonFloatValue buttonClicked

                newDismantledNum =
                    buildTotal
                        { dismantledNumShortcut
                            | beforeDecimal = newNum
                            , afterDecimal = 0
                        }
            in
            { model
                | display = showResults newDismantledNum.total
                , displayStatus = NotResults
                , dismantledNum = newDismantledNum
            }

        ( ShowingResults, Decimal, _ ) ->
            concatNumbers
                { model
                    | decimalStatus = ClickedDecimal
                    , displayStatus = NotResults
                    , dismantledNum =
                        { dismantledNumShortcut
                            | beforeDecimal = 0
                            , afterDecimal = 0
                        }
                }
                (buttonFloatValue buttonClicked)

        ( NotResults, Num, _ ) ->
            concatNumbers
                { model
                    | displayStatus = NotResults
                }
                (buttonFloatValue buttonClicked)

        ( NotResults, Decimal, NoDecimal ) ->
            concatNumbers
                { model | decimalStatus = ClickedDecimal }
                (buttonFloatValue buttonClicked)

        ( NotResults, Decimal, HasDecimal ) ->
            model

        ( NotResults, Decimal, ClickedDecimal ) ->
            concatNumbers
                { model | decimalStatus = HasDecimal }
                (buttonFloatValue buttonClicked)


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
        , div [] [ text "beforeDecimal : ", text <| String.fromFloat model.dismantledNum.beforeDecimal ]
        , div [] [ text "afterDecimal : ", text <| String.fromFloat model.dismantledNum.afterDecimal ]
        , div [] [ text "full number : ", text <| String.fromFloat model.dismantledNum.total ]
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
        HasDecimal ->
            "HasDecimal"

        NoDecimal ->
            "NoDecimal"

        ClickedDecimal ->
            "ClickedDecimal"


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
