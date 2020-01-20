module RippleCarryAdder exposing (Binary, andGate, digits, fullAdder, halfAdder, inverter, orGate, rippleCarryAdder)

import Array
import Bitwise


andGate : Int -> Int -> Int
andGate a b =
    Bitwise.and a b


orGate : Int -> Int -> Int
orGate a b =
    Bitwise.or a b


inverter : Int -> Int
inverter a =
    case a of
        0 ->
            1

        1 ->
            0

        _ ->
            -1


halfAdder : Int -> Int -> { carry : Int, sum : Int }
halfAdder a b =
    let
        d =
            orGate a b

        e =
            andGate a b |> inverter

        sumDigit =
            andGate d e

        carryOut =
            andGate a b
    in
    { carry = carryOut
    , sum = sumDigit
    }


fullAdder : Int -> Int -> Int -> { carry : Int, sum : Int }
fullAdder a b carryIn =
    let
        firstResult =
            halfAdder b carryIn

        secondResult =
            halfAdder a firstResult.sum

        finalCarry =
            orGate firstResult.carry secondResult.carry
    in
    { carry = finalCarry
    , sum = secondResult.sum
    }


type alias Binary =
    { d0 : Int
    , d1 : Int
    , d2 : Int
    , d3 : Int
    }


extractDigits : Int -> Binary
extractDigits number =
    digits number
        |> padZeros 4
        |> Array.fromList
        |> arrayToRecord


stringToInt : String -> Int
stringToInt str =
    String.toInt str |> Maybe.withDefault 0


arrayToRecord : Array.Array Int -> Binary
arrayToRecord array =
    let
        d0 =
            Array.get 0 array |> Maybe.withDefault 0

        d1 =
            Array.get 1 array |> Maybe.withDefault 0

        d2 =
            Array.get 2 array |> Maybe.withDefault 0

        d3 =
            Array.get 3 array |> Maybe.withDefault 0
    in
    Binary d0 d1 d2 d3


numberFromDigits : List Int -> Int
numberFromDigits digitList =
    List.foldl (\digit number -> digit + number * 10) 0 digitList


digits : Int -> List Int
digits number =
    let
        getDigits n =
            if n == 0 then
                []

            else
                remainderBy 10 n :: getDigits (n // 10)
    in
    getDigits number
        |> List.reverse


rippleCarryAdder : Int -> Int -> Int -> Int
rippleCarryAdder a b carryIn =
    let
        firstSignal =
            extractDigits a

        secondSignal =
            extractDigits b

        firstResult =
            fullAdder firstSignal.d3 secondSignal.d3 carryIn

        secondResult =
            fullAdder firstSignal.d2 secondSignal.d2 firstResult.carry

        thirdResult =
            fullAdder firstSignal.d1 secondSignal.d1 secondResult.carry

        finalResult =
            fullAdder firstSignal.d0 secondSignal.d0 thirdResult.carry
    in
    [ finalResult, thirdResult, secondResult, firstResult ]
        |> List.map .sum
        |> (::) finalResult.carry
        |> numberFromDigits


padZeros : Int -> List Int -> List Int
padZeros total list =
    let
        numberOfZeros =
            total - List.length list
    in
    List.repeat numberOfZeros 0 ++ list
