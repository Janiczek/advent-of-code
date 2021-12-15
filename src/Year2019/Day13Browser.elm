module Year2019.Day13Browser exposing (main)

import Browser
import Dict exposing (Dict)
import Fifo
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import List.Extra
import List.Zipper as Zipper exposing (Zipper)
import Year2019.Day13
    exposing
        ( Coord
        , Joystick(..)
        , Output(..)
        , State
        , Tile(..)
        , input_
        , joystickToInt
        , parse2
        , parseObject
        , updateWithOutput
        )
import Year2019.Intcode as Intcode
    exposing
        ( Computer
        , OutputError(..)
        , Stop(..)
        )
import Year2019.Intcode.Memory as Memory exposing (Memory)


init : Memory -> Zipper State
init mem =
    { objects = Dict.empty
    , score = 0
    , computer =
        mem
            |> Memory.setMany [ ( 0, 2 ) ]
            |> Intcode.initWithMemory
            |> Intcode.stepUntilStopped
    }
        |> dealWithOutputs
        |> Zipper.singleton


dealWithOutputs : State -> State
dealWithOutputs state =
    let
        ( intOutputs, newComputer ) =
            Intcode.getOutputs state.computer

        outputs : List Output
        outputs =
            intOutputs
                |> List.Extra.groupsOf 3
                |> List.map parseObject
    in
    List.foldl
        updateWithOutput
        { state | computer = newComputer }
        outputs


type Msg
    = Move Joystick
    | Undo
    | Redo


main : Program () (Zipper State) Msg
main =
    Browser.sandbox
        { init = init (parse2 input_)
        , update = update
        , view = view
        }


{-|

    A [ B ] C D

    -->
    A B [ x ] C D

-}
insertNext : State -> Zipper State -> Zipper State
insertNext state zipper =
    zipper
        |> Zipper.mapAfter ((::) state)
        |> Zipper.next
        |> Maybe.withDefault zipper


update : Msg -> Zipper State -> Zipper State
update msg zipper =
    case msg of
        Move joystick ->
            zipper
                |> Zipper.mapAfter (always [])
                |> insertNext
                    (zipper
                        |> Zipper.current
                        |> addInputAndRun (joystickToInt joystick)
                        |> dealWithOutputs
                    )

        Undo ->
            Zipper.previous zipper
                |> Maybe.withDefault zipper

        Redo ->
            Zipper.next zipper
                |> Maybe.withDefault zipper


addInputAndRun : Int -> State -> State
addInputAndRun input state =
    { state
        | computer =
            case state.computer of
                Err (WaitsForInput computer) ->
                    computer
                        |> Intcode.addInput input
                        |> Intcode.stepUntilStopped

                Err (Halted computer) ->
                    state.computer

                other ->
                    Debug.todo <| "addInput ??? " ++ Debug.toString other
    }


view : Zipper State -> Html Msg
view zipper =
    let
        current =
            Zipper.current zipper
    in
    Html.div []
        [ viewScore current.score
        , viewJoystickButtons
        , viewUndo zipper
        , viewObjects current.objects
        ]


viewScore : Int -> Html Msg
viewScore score =
    Html.div
        []
        [ Html.text (String.fromInt score) ]


viewJoystickButtons : Html Msg
viewJoystickButtons =
    Html.div
        []
        [ Html.button [ Events.onClick (Move Left) ] [ Html.text "<-" ]
        , Html.button [ Events.onClick (Move Neutral) ] [ Html.text "o" ]
        , Html.button [ Events.onClick (Move Right) ] [ Html.text "->" ]
        ]


viewUndo : Zipper State -> Html Msg
viewUndo zipper =
    let
        hasNoPrevious =
            List.isEmpty (Zipper.before zipper)

        hasNoNext =
            List.isEmpty (Zipper.after zipper)
    in
    Html.div
        []
        [ Html.button
            [ Events.onClick Undo
            , Attrs.disabled hasNoPrevious
            ]
            [ Html.text "Undo" ]
        , Html.button
            [ Events.onClick Redo
            , Attrs.disabled hasNoNext
            ]
            [ Html.text "Redo" ]
        ]


viewObjects : Dict Coord Tile -> Html Msg
viewObjects objects =
    objects
        |> Dict.toList
        |> List.map viewObject
        |> Html.div
            [ Attrs.style "position" "relative" ]


viewObject : ( Coord, Tile ) -> Html Msg
viewObject ( ( x, y ), tile ) =
    Html.div
        [ Attrs.style "position" "absolute"
        , Attrs.style "left" (String.fromInt (10 * x) ++ "px")
        , Attrs.style "top" (String.fromInt (10 * y) ++ "px")
        , Attrs.style "width" "10px"
        , Attrs.style "height" "10px"
        , Attrs.style "background-color" (tileToColor tile)
        ]
        []


tileToColor : Tile -> String
tileToColor tile =
    case tile of
        Empty ->
            "transparent"

        Wall ->
            "brown"

        Block ->
            "red"

        HorizontalPaddle ->
            "blue"

        Ball ->
            "green"
