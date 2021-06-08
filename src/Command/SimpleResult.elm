module SimpleResult exposing (..)


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
            case String.split " " input of
                [ "PLACE", placeArgs ] ->
                    case splitIntoThree ',' placeArgs of
                        Just ( xStr, yStr, dirStr ) ->
                            case
                                ( xStr |> stringToBoundedInt
                                , yStr |> stringToBoundedInt
                                , dirStr |> parseDirection
                                )
                            of
                                ( Just x, Just y, Just dir ) ->
                                    Result.Ok (Place dir { x = x, y = y })

                                ( Nothing, _, _ ) ->
                                    Result.Err PlaceXInvalid

                                ( _, Nothing, _ ) ->
                                    Result.Err PlaceYInvalid

                                ( _, _, Nothing ) ->
                                    Result.Err PlaceDirectionInvalid

                        _ ->
                            Result.Err CantSplitIntoThree

                _ ->
                    Result.Err UnknownCommand
