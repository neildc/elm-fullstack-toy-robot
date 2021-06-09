module Command.JustMaybe exposing (parseCommand)

import Command.Deps exposing (..)


parseCommand : String -> Maybe Command
parseCommand input =
    case input of
        "MOVE" ->
            Just Move

        "LEFT" ->
            Just RotateLeft

        "RIGHT" ->
            Just RotateRight

        _ ->
            case getPlaceArgs input of
                Just placeArgs ->
                    case splitIntoThree ',' placeArgs of
                        Just ( xStr, yStr, dirStr ) ->
                            case
                                ( xStr |> stringToBoundedInt
                                , yStr |> stringToBoundedInt
                                , dirStr |> parseDirection
                                )
                            of
                                ( Just x, Just y, Just dir ) ->
                                    Just (Place dir { x = x, y = y })

                                _ ->
                                    Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing
