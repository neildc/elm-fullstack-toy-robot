module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Command
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class, src, style)
import Html.Events
import Keyboard
import Keyboard.Events
import Lamdera
import Time
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


initRobot =
    { position =
        { x = makeBoundedInt 2 |> Maybe.withDefault Types.defaultBoundedInt
        , y = makeBoundedInt 2 |> Maybe.withDefault Types.defaultBoundedInt
        }
    , direction = North
    }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , message = "Welcome to Lamdera! You're looking at the auto-generated base implementation. Check out src/Frontend.elm to start coding!"
      , robot = initRobot
      , inputText = ""
      , parseError = Nothing
      , currentTime = Time.millisToPosix 0
      , updatedAt = Time.millisToPosix 0
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
                , updatedAt = model.currentTime
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
                            , updatedAt = model.currentTime
                            , commandHistory = RemoteText command :: model.commandHistory
                          }
                        , Cmd.none
                        )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        Tick time ->
            ( { model | currentTime = time }, Cmd.none )

        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
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
                , updatedAt = model.currentTime
                , commandHistory = Keyboard direction :: model.commandHistory
              }
            , Lamdera.sendToBackend <| UpdateRobot command
            )

        UpdateInputText text ->
            ( { model | inputText = text, parseError = Nothing }
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
                        , updatedAt = model.currentTime
                        , commandHistory = LocalText model.inputText command :: model.commandHistory
                      }
                    , Lamdera.sendToBackend <| UpdateRobot command
                    )

                Result.Err err ->
                    ( { model | parseError = Just <| Command.parseErrorToString err }
                    , Cmd.none
                    )



-- else
--     ( model, Cmd.none )


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


viewRobot : Robot -> Html Never
viewRobot { direction, position } =
    let
        numberOfBoxesToShiftBy curr =
            negate Types.const_MAX_XorY + curr

        -- Now since I wanted to animate the movement of the bot...
        --
        -- Instead of simply conditionally rendering the bot into the box that
        -- corresponds to the currentPosition, we render the bot in the same box every cycle,
        -- then we translate it over based on the currentPosition
        --
        -- We have to calculate the displacment for the owl since
        -- the size of the boxes is dynamic to allow the page to responsive
        -- and the gridSize is also configurable.
        --
        -- Luckily CSS transform's allow you to define the displacement as a percentage of what
        -- you are moving, which works out well since we render the bots <img> as width/height=100%
        -- and it's parent is simply one of those responsive boxes.
        --
        -- There is the caveat that this percentage doesn't include the margin, so we must
        -- do an additional translation for that.
        -- Which is a lot simpler since we just hardcode the value in const_BOX_MARGIN_PX
        boxesDisplacement curr =
            let
                toPercentage d =
                    d
                        -- Margins around the box will be a percentage of the size of the box
                        * 100
                        |> String.fromInt
                        |> (\s -> s ++ "%")
            in
            numberOfBoxesToShiftBy curr |> toPercentage

        marginsDisplacement curr =
            numberOfBoxesToShiftBy curr
                * const_BOX_MARGIN_PX
                * 2
                |> String.fromInt
                |> (\s -> s ++ "px")

        rotationAngle =
            case direction of
                North ->
                    0

                East ->
                    90

                West ->
                    -90

                South ->
                    180

        ( x, y ) =
            ( getBoundedInt position.x, getBoundedInt position.y )
    in
    Html.img
        [ src "rocket.png"
        , style "width" "100%"
        , style "height" "100%"
        , style "transform" <|
            String.join " "
                [ "translateX(" ++ boxesDisplacement x ++ ")"
                , "translateX(" ++ marginsDisplacement x ++ ")"
                , "translateY(" ++ boxesDisplacement y ++ ")"
                , "translateY(" ++ marginsDisplacement y ++ ")"

                -- Make sure that the rotation is applied first
                --
                -- CSS transform  applies these in reverse order of their listing
                -- if you rotate after you translate then the center/origin of the plane
                -- is no longer where the robot will be.
                --
                -- This is probably why you lost your car keys...
                , "rotate(" ++ String.fromInt rotationAngle ++ "deg)"
                ]
        , style "transition" "transform 0.5s" -- At least CSS does all the animating for free
        ]
        []


viewDirectionCluster : Html FrontendMsg
viewDirectionCluster =
    let
        directionButton direction =
            Html.button [ Html.Events.onClick <| HandleKeyPress direction ]
                [ Html.text <|
                    case direction of
                        North ->
                            "N"

                        West ->
                            "W"

                        East ->
                            "E"

                        South ->
                            "S"
                ]
    in
    div
        [ Keyboard.Events.on Keyboard.Events.Keydown <|
            List.map (Tuple.mapSecond HandleKeyPress)
                [ ( Keyboard.ArrowUp, North )
                , ( Keyboard.ArrowRight, East )
                , ( Keyboard.ArrowDown, South )
                , ( Keyboard.ArrowLeft, West )
                ]
        , Attr.tabindex 0
        , style "text-align" "center"
        , style "width" "200px"
        ]
        [ directionButton North
        , div []
            [ directionButton West
            , Html.text "+"
            , directionButton East
            ]
        , directionButton South
        ]


view : Model -> Html FrontendMsg
view model =
    let
        box { withRobot } =
            Html.div
                [ style "background" "lightgray"
                , style "height" "100px"
                , style "width" "100px"
                ]
                [ if withRobot then
                    viewRobot model.robot

                  else
                    Html.text ""
                ]

        numCells =
            (Types.const_MAX_XorY + 1) ^ 2

        grid =
            div
                [ style "display" "grid"
                , style "grid-template-columns" "repeat(5, 100px)"
                , style "grid-gap" "10px"
                ]
            <|
                List.map
                    (\row ->
                        box
                            { withRobot =
                                -- Render it last so it can be rendered over
                                -- all the other boxes/div nodes
                                -- (col == 4) &&
                                row == numCells
                            }
                    )
                    (List.range 1 numCells)
    in
    case model.clientId of
        Nothing ->
            Html.h1 [] [ Html.text "Loading history..." ]

        Just _ ->
            div
                [ style "display" "flex"
                , style "flex" "row"
                , style "margin" "20px"
                ]
                [ div
                    [ Keyboard.Events.on Keyboard.Events.Keydown <|
                        List.map (Tuple.mapSecond HandleKeyPress)
                            [ ( Keyboard.ArrowUp, North )
                            , ( Keyboard.ArrowRight, East )
                            , ( Keyboard.ArrowDown, South )
                            , ( Keyboard.ArrowLeft, West )
                            ]
                    , Attr.tabindex 0
                    ]
                    [ Html.div [] [ Html.map never grid ]
                    , Html.text "Use the following buttons to rotate and move, alternatively use the arrow keys."
                    , viewDirectionCluster
                    , viewParser model
                    ]
                , div [ style "padding-left" "30px" ]
                    [ viewCommandHistory model.commandHistory ]
                ]


const_BOX_MARGIN_PX : Int
const_BOX_MARGIN_PX =
    5


viewParser : Model -> Html FrontendMsg
viewParser model =
    Html.div []
        [ Html.text <| ""
        , Html.h3 [] [ Html.text "Commands" ]
        , Html.div [] <|
            List.map (\t -> Html.p [] [ Html.text t ])
                [ "PLACE X,Y,[NORTH,EAST,SOUTH,WEST]"
                , "MOVE"
                , "LEFT"
                , "RIGHT"
                ]
        , Html.div []
            [ Html.input
                [ Html.Events.onInput UpdateInputText
                ]
                []
            , Html.button
                [ Html.Events.onClick ParseAndExecuteCommand ]
                [ Html.text "GO" ]
            ]
        , Html.div []
            [ Html.text <| (model.parseError |> Maybe.withDefault "")
            ]
        ]


viewCommandHistory : List CommandSource -> Html FrontendMsg
viewCommandHistory commandHistory =
    let
        commandSourceToString cs =
            case cs of
                Keyboard dir ->
                    "Keyboard"

                -- TODO
                LocalText inputText command ->
                    "Local Command: " ++ inputText ++ "/" ++ commandToString command

                RemoteText command ->
                    "Remote" ++ commandToString command

        commandToString c =
            case c of
                Place dir pos ->
                    "Place"

                -- TODO
                RotateLeft ->
                    "Rotate Left"

                RotateRight ->
                    "Rotate Right"

                Move ->
                    "Move"

        viewLog c =
            Html.p []
                [ Html.text <| commandSourceToString c
                ]
    in
    Html.div
        [ style "height" "90vh"
        , style "max-height" "90vh"
        ]
        [ Html.h1 [] [ Html.text <| "HISTORY" ]
        , Html.div [ style "overflow-y" "scroll", style "max-height" "85vh" ] <|
            List.map viewLog commandHistory
        ]


viewSimple : Model -> Html FrontendMsg
viewSimple model =
    let
        box { withRobot } =
            Element.el
                [ Element.width <| Element.px 100
                , Element.height <| Element.px 100
                , Element.centerX
                , Element.centerY
                , Background.color <| Element.rgb 100 1 0
                , Border.color <| Element.rgb 255 255 255
                , Border.solid
                ]
            <|
                Element.text <|
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

        grid =
            Element.column
                [ Element.spacing 10, Element.centerY ]
            <|
                List.map
                    (\row ->
                        Element.row
                            [ Element.spacing 10, Element.centerY ]
                        <|
                            List.map
                                (\col ->
                                    box
                                        { withRobot =
                                            -- Render it last so it can be rendered over
                                            -- all the other boxes/div nodes
                                            (col == getBoundedInt model.robot.position.x)
                                                && (row == getBoundedInt model.robot.position.y)
                                        }
                                )
                                (List.range 0 <| Types.const_MAX_XorY)
                    )
                    (List.range 0 <| Types.const_MAX_XorY)
    in
    div
        [ Keyboard.Events.on Keyboard.Events.Keydown <|
            List.map (Tuple.mapSecond HandleKeyPress)
                [ ( Keyboard.ArrowUp, North )
                , ( Keyboard.ArrowRight, East )
                , ( Keyboard.ArrowDown, South )
                , ( Keyboard.ArrowLeft, West )
                ]
        , Attr.tabindex 0
        ]
        [ Element.layout [ Element.centerX, Element.centerY ] <| Element.map never grid
        , Html.text "Use the following buttons to rotate and move the owl, alternatively use the arrow keys."
        , viewDirectionCluster
        , viewParser model
        ]


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view >> (\v -> { title = "", body = [ v ] })
        }


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Time.every 1000 Tick
