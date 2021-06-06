module Command exposing
    ( ParseError
    , parse
    , parseErrorToString
    )

import Types
    exposing
        ( BoundedInt
        , Command(..)
        , Direction(..)
        , Position
        , makeBoundedInt
        )


type ParseError
    = UnknownCommand
    | CantSplitIntoThree
    | PlaceDirectionInvalid
    | PlaceXInvalid
    | PlaceYInvalid


parse : String -> Result ParseError Command
parse input =
    case input of
        "MOVE" ->
            Result.Ok Move

        "LEFT" ->
            Result.Ok RotateLeft

        "RIGHT" ->
            Result.Ok RotateRight

        _ ->
            let
                finishIt : BoundedInt -> BoundedInt -> Direction -> Command
                finishIt x y dir =
                    Place dir { x = x, y = y }
            in
            getPlaceArgs input
                |> Result.fromMaybe UnknownCommand
                |> Result.andThen (splitIntoThree >> Result.fromMaybe CantSplitIntoThree)
                |> Result.andThen
                    (\( xStr, yStr, dirStr ) ->
                        Result.map3
                            finishIt
                            (xStr |> stringToIntThenMakeBoundedInt |> Result.fromMaybe PlaceXInvalid)
                            (yStr |> stringToIntThenMakeBoundedInt |> Result.fromMaybe PlaceYInvalid)
                            (parseDirection dirStr |> Result.fromMaybe PlaceDirectionInvalid)
                    )


getPlaceArgs : String -> Maybe String
getPlaceArgs input =
    case String.split " " input of
        [ "PLACE", placeArgs ] ->
            Just placeArgs

        _ ->
            Nothing


stringToIntThenMakeBoundedInt : String -> Maybe BoundedInt
stringToIntThenMakeBoundedInt string =
    string |> String.toInt |> Maybe.andThen makeBoundedInt


splitIntoThree : String -> Maybe ( String, String, String )
splitIntoThree input =
    case String.split "," input of
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


parseErrorToString err =
    case err of
        UnknownCommand ->
            "Unknown Command"

        CantSplitIntoThree ->
            "Can't split"

        PlaceDirectionInvalid ->
            "Place direction is invalid"

        PlaceXInvalid ->
            "Place X is invalid"

        PlaceYInvalid ->
            "Place Y is invalid"
