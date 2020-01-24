module RippleCarryAdderTests exposing (rippleCarryAdderFuzzTests)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import RippleCarryAdder exposing (..)
import Test exposing (..)


rippleCarryAdderFuzzTests : Test
rippleCarryAdderFuzzTests =
    describe "carry-out relationship with the most significant digits"
        [ fuzz3
            (list (intRange 0 1))
            (list (intRange 0 1))
            (intRange 0 1)
            "carry-out is 0 when most significant digits are both 0"
          <|
            \list1 list2 carryIn ->
                let
                    convertToBinnary digitsList =
                        digitsList
                            |> List.take 3
                            |> numberFromDigits

                    firstInput =
                        convertToBinnary list1

                    secondInput =
                        convertToBinnary list2
                in
                rippleCarryAdder firstInput secondInput carryIn
                    |> Expect.atMost 1111
        , fuzz3
            (list (intRange 0 1))
            (list (intRange 0 1))
            (intRange 0 1)
            "carry-out is 1 when most significant digits are both 1"
          <|
            \list1 list2 carryIn ->
                let
                    convertToBinnary digitsList =
                        digitsList
                            |> (::) 1
                            |> numberFromDigits

                    firstInput =
                        convertToBinnary list1

                    secondInput =
                        convertToBinnary list2
                in
                rippleCarryAdder firstInput secondInput carryIn
                    |> Expect.atMost 11111
        , fuzz3
            (list (intRange 0 1))
            (list (intRange 0 1))
            (constant 0)
            """
            the least singnificant digit of the output is always 0
            when the least significant digits of inputs are 0 and
            carry-in is 0
            """
          <|
            \list1 list2 carryIn ->
                let
                    convertToBinary digitList =
                        digitList
                            |> List.take 3
                            |> List.reverse
                            |> List.append [ 0 ]
                            |> List.reverse
                            |> numberFromDigits

                    firstInput =
                        convertToBinary list1

                    secondInput =
                        convertToBinary list2

                    isLastDigitZero : List number -> Bool
                    isLastDigitZero digitList =
                        if List.length digitList == 1 then
                            if digitList == [ 0 ] then
                                True

                            else
                                False

                        else
                            isLastDigitZero (Maybe.withDefault [ 0 ] (List.tail digitList))
                in
                rippleCarryAdder firstInput secondInput carryIn
                    |> digits
                    |> isLastDigitZero
                    |> Expect.equal True
        , fuzz3
            (list (intRange 0 1))
            (list (intRange 0 1))
            (constant 0)
            """
            the least singnificant digit of the output is always 0
            when the least significant digits of inputs are 1 and
            carry-in is 0
            """
          <|
            \list1 list2 carryIn ->
                let
                    convertToBinary digitList =
                        digitList
                            |> List.take 3
                            |> List.reverse
                            |> List.append [ 1 ]
                            |> List.reverse
                            |> numberFromDigits

                    firstInput =
                        convertToBinary list1

                    secondInput =
                        convertToBinary list2

                    isLastDigitZero digitList =
                        case digitList of
                            [ 0 ] ->
                                True

                            [ 1 ] ->
                                False

                            [] ->
                                False

                            _ :: xs ->
                                isLastDigitZero xs
                in
                rippleCarryAdder firstInput secondInput carryIn
                    |> digits
                    |> isLastDigitZero
                    |> Expect.equal True
        ]


numberFromDigits : List number -> number
numberFromDigits digitList =
    List.foldl (\digit number -> number * 10 + digit) 0 digitList


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
