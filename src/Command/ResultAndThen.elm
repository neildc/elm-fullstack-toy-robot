module ResultAndThen exposing (ParseError, parseCommand)


type Result error value
    = Ok value
    | Err error


type ParseError
    = UnknownCommand
    | CantSplitIntoThree
    | PlaceDirectionInvalid
    | PlaceXInvalid
    | PlaceYInvalid


parseCommand : String -> Result ParseError Command
parseCommand input =
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
                |> Result.andThen (splitIntoThree ',' >> Result.fromMaybe CantSplitIntoThree)
                |> Result.andThen
                    (\( xStr, yStr, dirStr ) ->
                        Result.map3
                            (\x y dir -> Place dir { x = x, y = y })
                            (xStr |> stringToBoundedInt |> Result.fromMaybe PlaceXInvalid)
                            (yStr |> stringToBoundedInt |> Result.fromMaybe PlaceYInvalid)
                            (parseDirection dirStr |> Result.fromMaybe PlaceDirectionInvalid)
                    )
