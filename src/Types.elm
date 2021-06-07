module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , CommandSource(..)
    , FrontendModel
    , FrontendMsg(..)
    , Robot
    , ToBackend(..)
    , ToFrontend(..)
    )

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Command exposing (Command)
import Lamdera exposing (ClientId, SessionId)
import Time
import Types.Direction exposing (Direction)
import Types.Position exposing (Position)
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
