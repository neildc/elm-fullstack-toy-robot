module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Lamdera
import Time
import Url


type BoundedInt
    = BoundedInt Int


type alias Position =
    { x : BoundedInt
    , y : BoundedInt
    }


type Direction
    = North
    | East
    | West
    | South


type alias Robot =
    { position : Position
    , direction : Direction
    }


type Command
    = Place Direction Position
    | RotateLeft
    | RotateRight
    | Move


type CommandSource
    = Keyboard Direction
    | LocalText Command
    | RemoteText Command


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , message : String
    , robot : Robot
    , inputText : String
    , parseError : Maybe String
    , currentTime : Time.Posix
    , updatedAt : Time.Posix
    , commandHistory : List CommandSource
    , clientId : Maybe Lamdera.ClientId
    }


type alias BackendModel =
    { commandHistory : List Command
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | UpdateInputText String
    | ParseAndExecuteCommand
    | HandleKeyPress Direction
    | Tick Time.Posix


type ToBackend
    = UpdateRobot Command


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = BroadcastCommand Lamdera.SessionId Command
    | InitFromBackend Lamdera.SessionId (List Command)
