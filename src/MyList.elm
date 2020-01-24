module MyList exposing (MyList(..), isEmpty, sum)


type MyList a
    = Empty
    | Node a (MyList a)


sum : MyList Int -> Int
sum myList =
    case myList of
        Empty ->
            0

        Node intValue remainingValue ->
            intValue + sum remainingValue


isEmpty : MyList a -> Bool
isEmpty myList =
    case myList of
        Empty ->
            True

        _ ->
            False
