module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Command exposing (Command(..))
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class, src, style)
import Html.Events
import Keyboard
import Keyboard.Events
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
    { robot : Robot
    , inputText : String
    }


type CommandSource
    = Keyboard Direction
    | LocalText String Command
    | RemoteText Command


type alias Robot =
    { position : Position, direction : Direction }


initRobot =
    { position =
        { x = makeBoundedInt 2 |> Maybe.withDefault defaultBoundedInt
        , y = makeBoundedInt 2 |> Maybe.withDefault defaultBoundedInt
        }
    , direction = North
    }


init : Model
init =
    { robot = initRobot
    , inputText = ""
    }


type FrontendMsg
    = UpdateInputText String
    | ParseAndExecuteCommand
    | HandleKeyPress Direction


update msg model =
    case msg of
        HandleKeyPress direction ->
            let
                command =
                    if direction == model.robot.direction then
                        Move

                    else
                        Place direction model.robot.position
            in
            { model | robot = model.robot |> updateByCommand command }

        UpdateInputText text ->
            { model | inputText = text }

        ParseAndExecuteCommand ->
            case model.inputText |> Command.parse of
                Result.Ok command ->
                    let
                        newRobot =
                            updateByCommand command model.robot
                    in
                    { model | robot = newRobot }

                Result.Err err ->
                    model


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
            negate Types.Position.const_MAX_XorY + curr

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
                [ Html.text <| String.left 1 <| Types.Direction.toString direction ]
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
                , style "height" "6vw"
                , style "width" "6vw"
                ]
                [ if withRobot then
                    viewRobot model.robot

                  else
                    Html.text ""
                ]

        numCells =
            (Types.Position.const_MAX_XorY + 1) ^ 2

        grid =
            div
                [ style "display" "grid"
                , style "grid-template-columns" "repeat(5, 6vw)"
                , style "grid-gap" "10px"
                ]
                (List.map
                    (\row ->
                        box
                            { withRobot =
                                -- Render it last so it can be rendered over
                                -- all the other boxes/div nodes
                                row == numCells
                            }
                    )
                    (List.range 1 numCells)
                )
    in
    div
        [ style "display" "flex"
        , style "flex" "row"
        , style "margin" "20px"
        ]
        [ div
            [ Attr.tabindex 0
            , Keyboard.Events.on Keyboard.Events.Keydown <|
                List.map (Tuple.mapSecond HandleKeyPress)
                    [ ( Keyboard.ArrowUp, North )
                    , ( Keyboard.ArrowRight, East )
                    , ( Keyboard.ArrowDown, South )
                    , ( Keyboard.ArrowLeft, West )
                    ]
            ]
            [ Html.div [] [ Html.map never grid ]
            , Html.text "Use the following buttons to rotate and move, alternatively use the arrow keys."
            , viewDirectionCluster
            , viewParser model
            ]
        ]


const_BOX_MARGIN_PX : Int
const_BOX_MARGIN_PX =
    5


viewParser : Model -> Html FrontendMsg
viewParser model =
    Html.div []
        [ Html.h3 [] [ Html.text "Commands" ]
        , Html.div [] <|
            List.map (\t -> Html.p [] [ Html.text t ])
                [ "PLACE X,Y,[NORTH,EAST,SOUTH,WEST]"
                , "MOVE"
                , "LEFT"
                , "RIGHT"
                ]
        , div [ style "display" "flex", style "flex" "row" ]
            [ Html.form [ Html.Events.onSubmit ParseAndExecuteCommand ]
                [ Html.input
                    [ Html.Events.onInput UpdateInputText
                    ]
                    []
                ]
            , Html.button
                [ Html.Events.onClick ParseAndExecuteCommand ]
                [ Html.text "GO" ]
            ]
        , Html.text <|
            case model.inputText |> Command.parse of
                Result.Ok command ->
                    "Ok: " ++ Command.toString command

                Result.Err err ->
                    "Err: " ++ Command.parseErrorToString err
        ]


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
