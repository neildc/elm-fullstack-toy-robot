module Types.Direction exposing (..)


type Direction
    = North
    | East
    | West
    | South


toString dir =
    case dir of
        North ->
            "North"

        East ->
            "East"

        South ->
            "South"

        West ->
            "West"
