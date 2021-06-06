module Main exposing (..)

import Browser
import Command exposing (Command)
import Html exposing (Html, div)
import Html.Attributes exposing (class, src, style)
import Html.Events
import Keyboard
import Keyboard.Events
import Types exposing (BoundedInt, Direction(..), Position, getBoundedInt, makeBoundedInt)


main =
    Browser.sandbox
        { init = init, update = update, view = view }


type alias Model =
    { robot : Robot
    , inputText : String
    , parseError : Maybe String
    , commandLog : List CommandSource
    }


type alias Robot =
    { position : Position, direction : Direction }


init : Model
init =
    { robot =
        { position =
            { x = makeBoundedInt 2 |> Maybe.withDefault Types.defaultBoundedInt
            , y = makeBoundedInt 2 |> Maybe.withDefault Types.defaultBoundedInt
            }
        , direction = North
        }
    , inputText = ""
    , parseError = Nothing
    }


type Msg
    = UpdateInputText String
    | ParseAndExecuteCommand
    | HandleKeyPress Direction


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleKeyPress direction ->
            let
                newRobot =
                    if direction == model.robot.direction then
                        move model.robot

                    else
                        model.robot |> (\r -> { r | direction = direction })
            in
            { model
                | robot = newRobot
                , commandLog = model.commandLog |> List.append [ Keyboard direction ]
            }

        UpdateInputText text ->
            { model | inputText = text, parseError = Nothing }

        ParseAndExecuteCommand ->
            case model.inputText |> Command.parse of
                Result.Ok command ->
                    { model
                        | robot = updateByCommand command model.robot
                        , commandLog = model.commandLog |> List.append [ LocalText command ]
                    }

                Result.Err err ->
                    { model | parseError = Just <| Debug.toString err }


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
        Command.Place newDir newPos ->
            { position = newPos, direction = newDir }

        Command.Move ->
            move currentRobot

        Command.RotateRight ->
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

        Command.RotateLeft ->
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
        [ src "%PUBLIC_URL%/rocket.png"
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


viewDirectionCluster : Html Msg
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
        , Html.Attributes.tabindex 0
        ]
        [ directionButton North
        , div []
            [ directionButton West
            , Html.text "+"
            , directionButton East
            ]
        , directionButton South
        ]


view : Model -> Html Msg
view model =
    let
        box { withRobot } =
            div
                [ class "box"
                , style "margin" (String.fromInt const_BOX_MARGIN_PX ++ "px")
                ]
                [ div [ class "inner" ]
                    [ if withRobot then
                        viewRobot model.robot

                      else
                        Html.text ""
                    ]
                ]

        grid =
            div [ class "grid" ] <|
                List.map
                    (\row ->
                        div [ class "row" ] <|
                            List.map
                                (\col ->
                                    box
                                        { withRobot =
                                            -- Render it last so it can be rendered over
                                            -- all the other boxes/div nodes
                                            (col == Types.const_MAX_XorY)
                                                && (row == Types.const_MAX_XorY)
                                        }
                                )
                                (List.range 0 <| Types.const_MAX_XorY)
                    )
                    (List.range 0 <| Types.const_MAX_XorY)
    in
    div []
        [ Html.map never grid
        , Html.text "Use the following buttons to rotate and move the owl, alternatively use the arrow keys."
        , viewDirectionCluster
        , viewParser model
        ]


const_BOX_MARGIN_PX : Int
const_BOX_MARGIN_PX =
    5


viewParser : Model -> Html Msg
viewParser model =
    Html.div []
        [ Html.text <| Debug.toString model.robot
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
            [ Html.text <| (model.parseError |> Maybe.map Debug.toString |> Maybe.withDefault "")
            ]
        ]
