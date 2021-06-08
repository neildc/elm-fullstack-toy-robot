module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Command exposing (Command(..))
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html.Attributes as Attr
import Keyboard
import Keyboard.Events
import Lamdera
import Types exposing (..)
import Types.Direction exposing (Direction(..))
import Types.Position
    exposing
        ( BoundedInt
        , Position
        , defaultBoundedInt
        , getBoundedInt
        , makeBoundedInt
        )
import Url


type alias Model =
    FrontendModel


initRobot =
    { position =
        { x = makeBoundedInt 2 |> Maybe.withDefault defaultBoundedInt
        , y = makeBoundedInt 2 |> Maybe.withDefault defaultBoundedInt
        }
    , direction = North
    }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { robot = initRobot
      , inputText = ""
      , clientId = Nothing
      , commandHistory = []
      }
    , Cmd.none
    )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        InitFromBackend clientId commands ->
            ( { model
                | robot = commands |> List.foldr updateByCommand initRobot
                , clientId = Just clientId
                , commandHistory = List.map RemoteText commands
              }
            , Cmd.none
            )

        BroadcastCommand clientId command ->
            case model.clientId of
                Nothing ->
                    ( model, Cmd.none )

                Just c ->
                    if c == clientId then
                        ( model, Cmd.none )

                    else
                        ( { model
                            | robot = model.robot |> updateByCommand command
                            , commandHistory = RemoteText command :: model.commandHistory
                          }
                        , Cmd.none
                        )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                      -- , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    , Cmd.none
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        HandleKeyPress direction ->
            let
                command =
                    if direction == model.robot.direction then
                        Move

                    else
                        Place direction model.robot.position
            in
            ( { model
                | robot = model.robot |> updateByCommand command
                , commandHistory = Keyboard direction :: model.commandHistory
              }
            , Lamdera.sendToBackend <| UpdateRobot command
            )

        UpdateInputText text ->
            ( { model | inputText = text }
            , Cmd.none
            )

        ParseAndExecuteCommand ->
            case model.inputText |> Command.parse of
                Result.Ok command ->
                    let
                        newRobot =
                            updateByCommand command model.robot
                    in
                    ( { model
                        | robot = newRobot
                        , commandHistory = LocalText model.inputText command :: model.commandHistory
                      }
                    , Lamdera.sendToBackend <| UpdateRobot command
                    )

                Result.Err err ->
                    ( model, Cmd.none )


move : Robot -> Robot
move ({ position, direction } as currentRobot) =
    let
        p =
            position

        ( xOrY, setToXOrY, addOrMinus ) =
            case direction of
                North ->
                    ( .y, \y -> { p | y = y }, \y -> y - 1 )

                South ->
                    ( .y, \y -> { p | y = y }, \y -> y + 1 )

                East ->
                    ( .x, \x -> { p | x = x }, \x -> x + 1 )

                West ->
                    ( .x, \x -> { p | x = x }, \x -> x - 1 )

        attemptMove =
            position |> xOrY |> getBoundedInt |> addOrMinus |> makeBoundedInt
    in
    case attemptMove of
        Just newPosition ->
            { currentRobot | position = setToXOrY newPosition }

        Nothing ->
            currentRobot


updateByCommand : Command -> Robot -> Robot
updateByCommand cmd ({ position, direction } as currentRobot) =
    case cmd of
        Place newDir newPos ->
            { position = newPos, direction = newDir }

        Move ->
            move currentRobot

        RotateRight ->
            { currentRobot
                | direction =
                    case direction of
                        North ->
                            East

                        East ->
                            South

                        South ->
                            West

                        West ->
                            North
            }

        RotateLeft ->
            { currentRobot
                | direction =
                    case direction of
                        North ->
                            West

                        East ->
                            North

                        South ->
                            East

                        West ->
                            South
            }


const_BOX_MARGIN_PX : Int
const_BOX_MARGIN_PX =
    5


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = viewSimple >> (\v -> { title = "", body = [ v ] })
        }


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Sub.none


viewParser model =
    E.column [ E.spacing 15 ]
        [ E.el [] <| E.text "Commands"
        , E.textColumn [] <|
            List.map (\t -> E.paragraph [] [ E.text <| "- " ++ t ])
                [ "PLACE X,Y,[NORTH,EAST,SOUTH,WEST]"
                , "MOVE"
                , "LEFT"
                , "RIGHT"
                ]
        , E.row []
            [ Input.text []
                { onChange = UpdateInputText
                , text = model.inputText
                , placeholder = Nothing
                , label = Input.labelAbove [] <| E.text "Enter a Command"
                }
            , Input.button []
                { onPress = Just ParseAndExecuteCommand
                , label = E.text "GO"
                }
            ]
        , E.text <|
            case model.inputText |> Command.parse of
                Result.Ok command ->
                    "Ok: " ++ Command.toString command

                Result.Err err ->
                    "Err: " ++ Command.parseErrorToString err
        ]


viewSimple model =
    let
        viewCell { withRobot } =
            E.el
                [ E.width <| E.px 100
                , E.height <| E.px 100
                , E.centerX
                , E.centerY
                , Background.color <| E.rgb 100 1 0
                , Border.color <| E.rgb 0 0 0
                , Border.solid
                , Border.rounded 20
                , Border.width 5
                ]
                (E.el
                    [ E.centerX, E.centerY ]
                    (E.text <|
                        if withRobot == False then
                            ""

                        else
                            case model.robot.direction of
                                North ->
                                    "^"

                                South ->
                                    "v"

                                East ->
                                    ">"

                                West ->
                                    "<"
                    )
                )

        mapRange f =
            List.map f (List.range 0 <| Types.Position.const_MAX_XorY)

        viewRow rowIdx =
            E.row
                [ E.spacing 10, E.centerX, E.centerY ]
                (mapRange <|
                    \colIdx ->
                        viewCell
                            { withRobot =
                                (colIdx == getBoundedInt model.robot.position.x)
                                    && (rowIdx == getBoundedInt model.robot.position.y)
                            }
                )

        viewGrid =
            E.column
                [ E.spacing 10, E.centerX, E.centerY ]
                (mapRange viewRow)
    in
    E.layout
        [ E.centerX
        , E.centerY
        , E.htmlAttribute <|
            Keyboard.Events.on Keyboard.Events.Keydown <|
                List.map (Tuple.mapSecond HandleKeyPress)
                    [ ( Keyboard.ArrowUp, North )
                    , ( Keyboard.ArrowRight, East )
                    , ( Keyboard.ArrowDown, South )
                    , ( Keyboard.ArrowLeft, West )
                    ]
        , E.htmlAttribute <| Attr.tabindex 0
        ]
        (E.column [ E.centerX, E.centerY, E.spacing 30 ]
            [ E.row [ E.centerX, E.centerY, E.spacing 30 ]
                [ E.map never viewGrid
                , E.textColumn [] <|
                    List.map
                        (\c -> E.paragraph [] [ E.text <| commandSourceToString c ])
                        (List.take 20 model.commandHistory)
                ]
            , viewParser model
            ]
        )


commandSourceToString cs =
    case cs of
        Keyboard dir ->
            "Keyboard: " ++ Types.Direction.toString dir

        -- TODO
        LocalText inputText command ->
            "Local Command: " ++ inputText ++ " => " ++ Command.toString command

        RemoteText command ->
            "Remote: " ++ Command.toString command
