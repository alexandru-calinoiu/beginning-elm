module Playground exposing (main)

import Html
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


main =
    validateEmail "thedude@rubix.com"
        |> Debug.toString
        |> Html.text
