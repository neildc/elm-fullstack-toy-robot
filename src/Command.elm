module Command exposing
    ( Command(..)
    , ParseError
    , parse
    , parseErrorToString
    , toString
    )

import Types.Direction exposing (Direction(..))
import Types.Position exposing (BoundedInt, Position, makeBoundedInt)


type Command
    = Place Direction Position
    | RotateLeft
    | RotateRight
    | Move


toString c =
    case c of
        Place dir pos ->
            String.join " "
                [ "Place", Types.Direction.toString dir, Types.Position.toString pos ]

        RotateLeft ->
            "Rotate Left"

        RotateRight ->
            "Rotate Right"

        Move ->
            "Move"


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
            getPlaceArgs input
                |> Result.fromMaybe UnknownCommand
                |> Result.andThen (splitIntoThree >> Result.fromMaybe CantSplitIntoThree)
                |> Result.andThen
                    (\( xStr, yStr, dirStr ) ->
                        Result.map3
                            (\validX validY validDir -> Place validDir { x = validX, y = validY })
                            (xStr |> stringToBoundedInt |> Result.fromMaybe PlaceXInvalid)
                            (yStr |> stringToBoundedInt |> Result.fromMaybe PlaceYInvalid)
                            (parseDirection dirStr |> Result.fromMaybe PlaceDirectionInvalid)
                    )


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
