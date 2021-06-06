module Evergreen.Migrate.V3 exposing (..)

import Evergreen.V1.Types as Old
import Evergreen.V3.Types as New
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelMigrated
        ( { key = old.key
          , message = old.message
          , robot =
                New.Robot
                    { x = New.defaultBoundedInt, y = New.defaultBoundedInt }
                    New.North
          , inputText = old.inputText
          , parseError = old.parseError
          , currentTime = old.currentTime
          , updatedAt = old.updatedAt
          , commandHistory = []
          , clientId = Nothing
          }
        , Cmd.none
        )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgUnchanged
