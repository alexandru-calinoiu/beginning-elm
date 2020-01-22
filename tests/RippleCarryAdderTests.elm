module RippleCarryAdderTests exposing (halfAdderTests, inverterTests)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import RippleCarryAdder exposing (..)
import Test exposing (..)


inverterTests : Test
inverterTests =
    describe "Inverter"
        [ test "output is 0 when the input is 1" <|
            \_ ->
                inverter 0
                    |> Expect.equal 1
        , test "output is 1 when the input is 0" <|
            \_ ->
                inverter 1
                    |> Expect.equal 0
        ]


halfAdderTests : Test
halfAdderTests =
    describe "Half adder"
        [ test "sum and carry-out are 0 when both inputs are 0" <|
            \_ ->
                halfAdder 0 0
                    |> Expect.equal { sum = 0, carry = 0 }
        , test "sum is 0 and carry is 1 when both inputs are 1" <|
            \_ ->
                halfAdder 1 1
                    |> Expect.equal { sum = 0, carry = 1 }
        ]
