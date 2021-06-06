module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , BoundedInt
    , Command(..)
    , CommandSource(..)
    , Direction(..)
    , FrontendModel
    , FrontendMsg(..)
    , Position
    , Robot
    , ToBackend(..)
    , ToFrontend(..)
    , const_MAX_XorY
    , defaultBoundedInt
    , directionToString
    , getBoundedInt
    , makeBoundedInt
    , positionToString
    )

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    , robot : Robot
    , inputText : String
    , parseError : Maybe String
    , currentTime : Time.Posix
    , updatedAt : Time.Posix
    , commandHistory : List CommandSource
    , clientId : Maybe ClientId
    }


type Command
    = Place Direction Position
    | RotateLeft
    | RotateRight
    | Move


type CommandSource
    = Keyboard Direction
    | LocalText String Command
    | RemoteText Command


type alias Robot =
    { position : Position, direction : Direction }


type alias BackendModel =
    { commandHistory : List Command
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | UpdateInputText String
    | ParseAndExecuteCommand
    | HandleKeyPress Direction
    | Tick Time.Posix


type ToBackend
    = UpdateRobot Command


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected SessionId ClientId


type ToFrontend
    = BroadcastCommand SessionId Command
    | InitFromBackend SessionId (List Command)


type Direction
    = North
    | East
    | West
    | South


directionToString dir =
    case dir of
        North ->
            "North"

        East ->
            "East"

        South ->
            "South"

        West ->
            "West"


type alias Position =
    { x : BoundedInt, y : BoundedInt }


positionToString { x, y } =
    String.join ""
        [ "("
        , String.fromInt (getBoundedInt x)
        , ", "
        , String.fromInt (getBoundedInt y)
        , ")"
        ]


type BoundedInt
    = BoundedInt Int


const_MAX_XorY =
    4


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
