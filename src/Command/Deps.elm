module Command.Deps exposing
    ( Command(..)
    , getPlaceArgs
    , parseDirection
    , splitIntoThree
    , stringToBoundedInt
    )

import Types.Direction exposing (Direction(..))
import Types.Position exposing (BoundedInt, Position, makeBoundedInt)


type Command
    = Place Direction Position
    | RotateLeft
    | RotateRight
    | Move


getPlaceArgs : String -> Maybe String
getPlaceArgs input =
    case String.split " " input of
        [ "PLACE", placeArgs ] ->
            Just placeArgs

        _ ->
            Nothing


stringToBoundedInt : String -> Maybe BoundedInt
stringToBoundedInt string =
    string |> String.toInt |> Maybe.andThen makeBoundedInt


splitIntoThree : Char -> String -> Maybe ( String, String, String )
splitIntoThree delimeter input =
    let
        d =
            String.fromChar delimeter
    in
    case String.split d input of
        [ first, second, third ] ->
            Just ( first, second, third )

        _ ->
            Nothing


parseDirection : String -> Maybe Direction
parseDirection input =
    case input of
        "NORTH" ->
            Just North

        "EAST" ->
            Just East

        "WEST" ->
            Just West

        "SOUTH" ->
            Just South

        _ ->
            Nothing
