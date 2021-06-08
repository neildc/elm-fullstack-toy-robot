module Other exposing (..)


type Command
    = Place Direction Position
    | RotateLeft
    | RotateRight
    | Move


type alias Position =
    { x : BoundedInt, y : BoundedInt }


type Maybe a
    = Just a
    | Nothing


getPlaceArgs : String -> Maybe String


stringToBoundedInt : String -> Maybe BoundedInt


splitIntoThree : Char -> String -> Maybe ( String, String, String )


parseDirection : String -> Maybe Direction
