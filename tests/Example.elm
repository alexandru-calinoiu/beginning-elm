module Example exposing (additionTests, comparisonTests, guardianNames)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


additionTests : Test
additionTests =
    describe "Addition"
        [ test "two plus two equals four" <|
            \_ -> (2 + 2) |> Expect.equal 4
        , test "three plus four equals sever" <|
            \_ -> (3 + 4) |> Expect.equal 7
        ]


guardianNames : Test
guardianNames =
    test "only 2 guardians have names with less then 6 characters" <|
        \_ ->
            let
                guardians =
                    [ "Star-lord", "Groot", "Gamora", "Drax", "Rocket" ]
            in
            guardians
                |> List.map String.length
                |> List.filter (\length -> length < 6)
                |> List.length
                |> Expect.equal 2


comparisonTests : Test
comparisonTests =
    describe "Comparison"
        [ test "a list with 0 elemens is empty" <|
            \_ ->
                List.isEmpty []
                    |> Expect.true "expected the list to be empty"
        , test "6 is less than or equal to 7" <|
            \_ -> 6 |> Expect.atMost 7
        ]
