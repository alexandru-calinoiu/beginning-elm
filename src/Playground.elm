module Playground exposing
    ( Greeting(..)
    , Result(..)
    , User
    , arya
    , doubleScores
    , getAdultAge
    , guardiansWithShortNames
    , main
    , sansa
    , sayHello
    , scoresLessThan320
    , signUp
    , welcomeMessage
    )

import Html exposing (Html, text)
import MyList exposing (MyList(..), isEmpty)
import Regex


escapeEarth : Float -> Float -> String -> String
escapeEarth myVelocity mySpeed fuelStatus =
    let
        escapeVelocityInKmPerSec =
            11.186

        orbitSpeedInKmPerSec =
            7.67

        whereToLand =
            if fuelStatus == "low" then
                "Land on droneship"

            else
                "Land on lanuchpad"
    in
    if myVelocity > escapeVelocityInKmPerSec then
        "Goodspeed"

    else if mySpeed == orbitSpeedInKmPerSec then
        "Stay in orbit"

    else
        whereToLand


computedSpeed : Float -> Float -> Float
computedSpeed distance time =
    distance / time


computedTime : number -> number -> number
computedTime startTime endTime =
    endTime - startTime


weekday dayInNumber =
    case dayInNumber of
        0 ->
            "Sunday"

        1 ->
            "Monday"

        2 ->
            "Tuesday"

        3 ->
            "Wednesday"

        4 ->
            "Thursday"

        5 ->
            "Friday"

        6 ->
            "Saturday"

        _ ->
            "Unknown day"


hashtag : Int -> String
hashtag dayInNumber =
    case weekday dayInNumber of
        "Sunday" ->
            "#SinDay"

        "Monday" ->
            "#MondayBlues"

        "Tuesday" ->
            "#TakeMeBackTuesday"

        "Wednesday" ->
            "#HumpDay"

        "Thursday" ->
            "#ThrowbackThursday"

        "Friday" ->
            "#FlashbackFriday"

        "Saturday" ->
            "#Caturday"

        _ ->
            "#Whatever"


descending a b =
    case compare a b of
        LT ->
            GT

        GT ->
            LT

        EQ ->
            EQ


validateEmail : String -> ( String, String )
validateEmail email =
    let
        emailPattern =
            "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"

        regex =
            Maybe.withDefault Regex.never <| Regex.fromString emailPattern

        isValid =
            Regex.contains regex email
    in
    if isValid then
        ( "Valid email", "green" )

    else
        ( "Invaild email", "red" )


multiplyByFive : number -> number
multiplyByFive number =
    let
        multiplier =
            5
    in
    number * multiplier


doubleScores : List number -> List number
doubleScores scores =
    List.map ((*) 2) scores


scoresLessThan320 : List number -> List number
scoresLessThan320 scores =
    List.filter ((>) 320) scores


guardiansWithShortNames : List String -> Int
guardiansWithShortNames guardians =
    guardians
        |> List.map String.length
        |> List.filter (\x -> x < 6)
        |> List.length


type Result error value
    = Ok value
    | Err error


type Greeting
    = Howdy
    | Hola
    | Namaste String
    | NumericalHi Int Int


sayHello : Greeting -> String
sayHello greeting =
    case greeting of
        Howdy ->
            "How are you?"

        Hola ->
            "Hola amigo"

        Namaste message ->
            message

        NumericalHi value1 value2 ->
            value1 + value2 |> String.fromInt


signUp : String -> String -> Result String String
signUp email ageStr =
    case String.toInt ageStr of
        Nothing ->
            Err "Age must be an integer"

        Just age ->
            let
                emailPattern =
                    "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"

                regex =
                    Maybe.withDefault Regex.never <|
                        Regex.fromString emailPattern

                isValidEmail =
                    Regex.contains regex email
            in
            if age < 13 then
                Err "You need to be at least 13 years old to sign up"

            else if isValidEmail then
                Ok "Your account has been created successfully!"

            else
                Err "You entered an invalid email."


type alias Character =
    { name : String
    , age : Maybe Int
    }


sansa : Character
sansa =
    { name = "Sansa"
    , age = Just 19
    }


arya : Character
arya =
    { name = "Arya"
    , age = Nothing
    }


getAdultAge : Character -> Maybe Int
getAdultAge character =
    character.age
        |> Maybe.andThen
            (\age ->
                if age >= 18 then
                    Just age

                else
                    Nothing
            )


list1 : MyList a
list1 =
    Empty


list2 : MyList number
list2 =
    Node 9 Empty


type alias User =
    { name : String
    , email : String
    , age : Int
    , isLoggedIn : Bool
    }


welcomeMessage : { a | isLoggedIn : Bool, name : String } -> String
welcomeMessage { isLoggedIn, name } =
    case isLoggedIn of
        True ->
            "Welcome " ++ name ++ "!"

        False ->
            "Please log in."


main : Html.Html msg
main =
    isEmpty list2
        |> Debug.toString
        |> text
