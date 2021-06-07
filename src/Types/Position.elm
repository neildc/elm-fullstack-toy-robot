module Types.Position exposing
    ( BoundedInt
    , Position
    , const_MAX_XorY
    , defaultBoundedInt
    , getBoundedInt
    , makeBoundedInt
    , toString
    )


type alias Position =
    { x : BoundedInt, y : BoundedInt }


toString { x, y } =
    String.join ""
        [ "("
        , String.fromInt (getBoundedInt x)
        , ", "
        , String.fromInt (getBoundedInt y)
        , ")"
        ]


const_MAX_XorY =
    4


type BoundedInt
    = BoundedInt Int


defaultBoundedInt : BoundedInt
defaultBoundedInt =
    BoundedInt 0


makeBoundedInt : Int -> Maybe BoundedInt
makeBoundedInt int =
    if int >= 0 && int <= const_MAX_XorY then
        Just <| BoundedInt int

    else
        Nothing


getBoundedInt : BoundedInt -> Int
getBoundedInt (BoundedInt int) =
    int
