module FuzzTests exposing
    ( addOneTests
    , addTests
    , arrayGetTests
    , flipTests
    , listLengthTests
    , multiplyFloatTests
    , myListTests
    , pizzaLeftTests
    , stringTests
    )

import Array
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (..)
import MyList exposing (MyList(..), sum)
import Random exposing (maxInt, minInt)
import Test exposing (..)


addOneTests : Test
addOneTests =
    describe "addOne"
        [ fuzz frequencyFuzzer "adds 1 to any integer" <|
            \num ->
                addOne num |> Expect.equal (num + 1)
        ]


addTests : Test
addTests =
    describe "add"
        [ fuzz (tuple ( int, int )) "add two given integers" <|
            \( num1, num2 ) ->
                add num1 num2
                    |> Expect.equal (num1 + num2)
        ]


flipTests : Test
flipTests =
    describe "flip"
        [ fuzz bool "negates the given boolean value" <|
            \value ->
                flip value |> Expect.equal (not value)
        ]


multiplyFloatTests : Test
multiplyFloatTests =
    describe "multiplyFloat"
        [ fuzz2 (floatRange -1.0 1.0) int "muliplies given numbers" <|
            \x y ->
                multiplyFloat x y
                    |> Expect.within (Absolute 0.000000001) (x * toFloat y)
        ]


pizzaLeftTests : Test
pizzaLeftTests =
    describe "pizzaLeft"
        [ fuzz2 percentage float "returns remaining pizza slices" <|
            \eaten total ->
                pizzaLeft eaten total
                    |> Expect.within (Absolute 0.000000001) (total - (eaten * total))
        ]


stringTests : Test
stringTests =
    describe "The String module"
        [ fuzz string "restore the original string if aplied 2 times" <|
            \str ->
                str
                    |> String.reverse
                    |> String.reverse
                    |> Expect.equal str
        , fuzz2 (list int) int "does not remove a member of the list" <|
            \intList num ->
                intList
                    |> List.reverse
                    |> List.member num
                    |> Expect.equal (List.member num intList)
        ]


listLengthTests : Test
listLengthTests =
    describe "List.length"
        [ fuzz (list int) "never returns a negative value" <|
            \intList ->
                intList
                    |> List.length
                    |> Expect.atLeast 0
        ]


arrayGetTests : Test
arrayGetTests =
    describe "Array.get"
        [ fuzz (array (intRange -20 20)) "returns Nothing for out of range index" <|
            \intArray ->
                let
                    length =
                        Array.length intArray
                in
                intArray
                    |> Array.get length
                    |> Expect.equal Nothing
        ]


myListTests : Test
myListTests =
    describe "sum"
        [ test "sum for empty returns 0" <|
            \() ->
                sum MyList.Empty |> Expect.equal 0
        , fuzz (list int) "sum for any list of ints will return the sm of the elemens" <|
            \intList ->
                let
                    myList =
                        List.foldl MyList.Node MyList.Empty intList

                    sum =
                        List.sum intList
                in
                myList
                    |> MyList.sum
                    |> Expect.equal sum
        ]


frequencyFuzzer : Fuzzer Int
frequencyFuzzer =
    frequency
        [ ( 70, constant 7 )
        , ( 12, intRange 8 9 )
        , ( 6, constant 6 )
        , ( 9, intRange 2 4 )
        , ( 1, constant 5 )
        , ( 1, constant 1 )
        , ( 1, constant 1 )
        , ( 1, constant 10 )
        ]


addOne : Int -> Int
addOne x =
    1 + x


add : Int -> Int -> Int
add x y =
    x + y


flip : Bool -> Bool
flip x =
    not x


multiplyFloat : Float -> Int -> Float
multiplyFloat x y =
    x * toFloat y


pizzaLeft : Float -> Float -> Float
pizzaLeft eatenPercent totalSlices =
    totalSlices - (eatenPercent * totalSlices)
