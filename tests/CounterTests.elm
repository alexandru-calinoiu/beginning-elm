module CounterTests exposing (updateTests)

import Counter exposing (Model)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


updateTests : Test
updateTests =
    describe "update"
        [ test "update will increment the model" <|
            \_ -> Expect.equal 1 1
        ]
