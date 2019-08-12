module CalculatorButtonValues exposing (Action(..), ButtonType(..), CalButton(..), buttonToAction, buttonType, buttonValue)


type CalButton
    = CalNumOne
    | CalNumTwo
    | CalNumThree
    | CalNumFour
    | CalNumFive
    | CalNumSix
    | CalNumSeven
    | CalNumEight
    | CalNumNine
    | CalNumZero
    | CalDecimal
    | CalAdd
    | CalEqual


type ButtonType
    = Num
    | Decimal


type Action
    = Blank
    | Add
    | Equals


buttonType : CalButton -> ButtonType
buttonType button =
    case button of
        CalDecimal ->
            Decimal

        _ ->
            Num


buttonToAction : CalButton -> Action
buttonToAction button =
    case button of
        CalAdd ->
            Add

        CalEqual ->
            Equals

        _ ->
            Blank


buttonValue : CalButton -> String
buttonValue calButton =
    case calButton of
        CalNumOne ->
            "1"

        CalNumTwo ->
            "2"

        CalNumThree ->
            "3"

        CalNumFour ->
            "4"

        CalNumFive ->
            "5"

        CalNumSix ->
            "6"

        CalNumSeven ->
            "7"

        CalNumEight ->
            "8"

        CalNumNine ->
            "9"

        CalNumZero ->
            "0"

        CalDecimal ->
            "."

        CalAdd ->
            "+"

        CalEqual ->
            "="
