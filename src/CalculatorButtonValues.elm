module CalculatorButtonValues exposing (Action(..), ButtonType(..), CalButton(..), actionSymbolToString, buttonType, buttonValue)


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


type ButtonType
    = Num
    | Decimal


type Action
    = Add
    | Divide
    | Equals
    | Subtract
    | Multiply


actionSymbolToString : Action -> String
actionSymbolToString action =
    case action of
        Add ->
            "+"

        Divide ->
            "/"

        Equals ->
            "="

        Subtract ->
            "-"

        Multiply ->
            "*"


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
