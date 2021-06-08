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
import Time
import Types.Direction exposing (Direction)
import Types.Position exposing (Position)
import Url exposing (Url)


type alias BackendModel =
    { commandHistory : List Command
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | UpdateInputText String
    | ParseAndExecuteCommand
    | HandleKeyPress Direction


type ToBackend
    = UpdateRobot Command


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected SessionId ClientId


type ToFrontend
    = BroadcastCommand SessionId Command
    | InitFromBackend SessionId (List Command)
