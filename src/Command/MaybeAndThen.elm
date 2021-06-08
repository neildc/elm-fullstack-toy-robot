module MaybeAndThen exposing (..)


map : (a -> b) -> Maybe a -> Maybe b


andThen : (a -> Maybe b) -> Maybe a -> Maybe b


map3 : (a -> b -> c -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe value


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
            getPlaceArgs input
                |> Maybe.andThen (splitIntoThree ',')
                |> Maybe.andThen
                    (\( xStr, yStr, dirStr ) ->
                        Maybe.map3
                            (\x y dir -> Place dir { x = x, y = y })
                            (xStr |> stringToBoundedInt)
                            (yStr |> stringToBoundedInt)
                            (parseDirection dirStr)
                    )
