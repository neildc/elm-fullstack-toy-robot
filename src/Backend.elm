module Backend exposing (..)

import Html
import Lamdera exposing (ClientId, SessionId)
import Time
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { commandHistory = [] }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        -- A new client has joined! Send them history
        ClientConnected sessionId clientId ->
            ( model
            , Lamdera.sendToFrontend clientId (InitFromBackend clientId model.commandHistory)
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        UpdateRobot command ->
            ( { model | commandHistory = model.commandHistory ++ [ command ] }
            , Lamdera.broadcast <| BroadcastCommand clientId command
            )


subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        ]
