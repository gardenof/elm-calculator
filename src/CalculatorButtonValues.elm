module CalculatorButtonValues exposing (Action(..), ButtonType(..), CalButton(..), buttonType, buttonValue)


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
    | CalAction Action


type ButtonType
    = Num
    | Decimal


type Action
    = Add
    | Blank
    | Equals
    | Subtract
    | Multiply


buttonType : CalButton -> ButtonType
buttonType button =
    case button of
        CalDecimal ->
            Decimal

        _ ->
            Num


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

        CalAction Blank ->
            "Blank"

        CalAction Add ->
            "+"

        CalAction Equals ->
            "="

        CalAction Subtract ->
            "-"

        CalAction Multiply ->
            "*"
