module Year2019.Day11 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)
import List.Extra
import Year2019.Intcode as Intcode
    exposing
        ( Computer
        , OutputError(..)
        , Stop(..)
        )
import Year2019.Intcode.Memory as Memory exposing (Memory)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    Memory


type alias Input2 =
    Memory


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    Memory.fromString string
        |> Advent.unsafeMaybe "parse1"


parse2 : String -> Input2
parse2 string =
    Memory.fromString string
        |> Advent.unsafeMaybe "parse2"



-- 3. COMPUTE (actually solve the problem)


type Direction
    = Up
    | Down
    | Left
    | Right


type Color
    = Black
    | White


type alias State =
    { direction : Direction
    , position : ( Int, Int )
    , paint : Dict ( Int, Int ) Color
    , computer : Result Stop Computer
    }


initState1 : Memory -> State
initState1 mem =
    { direction = Up
    , position = ( 0, 0 )
    , paint = Dict.empty
    , computer = Ok <| Intcode.initWithMemory mem
    }


compute1 : Input1 -> Output1
compute1 mem =
    go (initState1 mem)
        |> .paint
        |> Dict.size


getColor : ( Int, Int ) -> Dict ( Int, Int ) Color -> Color
getColor coord paint_ =
    Dict.get coord paint_
        |> Maybe.withDefault Black


getCurrentColor : State -> Color
getCurrentColor state =
    getColor state.position state.paint


encodeColor : Color -> Int
encodeColor color =
    case color of
        Black ->
            0

        White ->
            1


runComputer : State -> State
runComputer state =
    { state
        | computer =
            state.computer
                |> Result.andThen Intcode.stepUntilStopped
    }


moveOnePanel : State -> State
moveOnePanel state =
    { state | position = move state.direction state.position }


move : Direction -> ( Int, Int ) -> ( Int, Int )
move direction ( x, y ) =
    case direction of
        Up ->
            ( x, y - 1 )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )


giveInput : Int -> State -> State
giveInput n state =
    { state
        | computer =
            state.computer
                |> Result.map (Intcode.addInput n)
    }


setComputer : Computer -> State -> State
setComputer computer state =
    { state | computer = Ok computer }


decodeColor : Int -> Color
decodeColor n =
    case n of
        0 ->
            Black

        1 ->
            White

        _ ->
            Debug.todo <| "decodeColor - got a weird color: " ++ String.fromInt n


paint : Color -> State -> State
paint color state =
    { state | paint = Dict.insert state.position color state.paint }


type Turn
    = TurnLeft
    | TurnRight


turn : Turn -> State -> State
turn turn_ state =
    { state
        | direction =
            case ( turn_, state.direction ) of
                ( TurnLeft, Up ) ->
                    Left

                ( TurnLeft, Left ) ->
                    Down

                ( TurnLeft, Down ) ->
                    Right

                ( TurnLeft, Right ) ->
                    Up

                ( TurnRight, Up ) ->
                    Right

                ( TurnRight, Right ) ->
                    Down

                ( TurnRight, Down ) ->
                    Left

                ( TurnRight, Left ) ->
                    Up
    }


decodeTurn : Int -> Turn
decodeTurn n =
    case n of
        0 ->
            TurnLeft

        1 ->
            TurnRight

        _ ->
            Debug.todo <| "decodeTurn - got a weird color: " ++ String.fromInt n


getOutput : Result Stop Computer -> Result OutputError ( Int, Computer )
getOutput computer =
    case computer of
        Ok computer_ ->
            Intcode.getOutput computer_

        Err (WaitsForInput computer_) ->
            Intcode.getOutput computer_

        Err err ->
            Debug.todo <| "getOutput - computer is stopped: " ++ printStop err


printStop : Stop -> String
printStop stop =
    case stop of
        UnknownOpcode op _ ->
            "Unknown opcode " ++ String.fromInt op

        Halted _ ->
            "Halted"

        WaitsForInput _ ->
            "Waits for input"


go : State -> State
go state =
    let
        stateAfterInputtingColor =
            state
                |> giveInput (encodeColor (getCurrentColor state))
                |> runComputer
    in
    case stateAfterInputtingColor.computer of
        Err (Halted _) ->
            stateAfterInputtingColor

        Err ((UnknownOpcode n _) as stop) ->
            Debug.todo <| "computer stopped: " ++ printStop stop

        _ ->
            let
                ( colorToPaint, computerAfterColor ) =
                    case getOutput stateAfterInputtingColor.computer of
                        Err NoOutputToGive ->
                            Debug.todo "go1 - no output to give for color"

                        Ok ( colorToPaint_, newComputer ) ->
                            ( decodeColor colorToPaint_, newComputer )

                stateAfterPainting =
                    stateAfterInputtingColor
                        |> setComputer computerAfterColor
                        |> paint colorToPaint

                ( directionToTurn, computerAfterTurn ) =
                    case getOutput stateAfterPainting.computer of
                        Err NoOutputToGive ->
                            Debug.todo "go1 - no output to give for direction"

                        Ok ( turn_, newComputer ) ->
                            ( decodeTurn turn_, newComputer )

                newState =
                    stateAfterPainting
                        |> setComputer computerAfterTurn
                        |> turn directionToTurn
                        |> moveOnePanel
            in
            go newState


initState2 : Memory -> State
initState2 mem =
    { direction = Up
    , position = ( 0, 0 )
    , paint = Dict.singleton ( 0, 0 ) White
    , computer = Ok <| Intcode.initWithMemory mem
    }


compute2 : Input2 -> Output2
compute2 mem =
    go (initState2 mem)
        |> .paint
        |> printHull
        |> always -1


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


printHull : Dict ( Int, Int ) Color -> ()
printHull dict =
    let
        maxX =
            Dict.foldl (\( x, _ ) _ currentMax -> max currentMax x) -1 dict

        maxY =
            Dict.foldl (\( _, y ) _ currentMax -> max currentMax y) -1 dict
    in
    List.range 0 maxY
        |> List.reverse
        |> List.map
            (\y ->
                List.range 0 maxX
                    |> List.map
                        (\x ->
                            case getColor ( x, y ) dict of
                                White ->
                                    ' '

                                Black ->
                                    '█'
                        )
                    |> String.fromList
                    |> Debug.log ""
            )
        |> always ()



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example"
        "input"
        Nothing -- Just "parsed-input"
        -1
     -}
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
3,8,1005,8,320,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,29,2,101,10,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,54,2,3,16,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,81,1006,0,75,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,101,0,8,105,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1001,8,0,128,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,149,1,105,5,10,1,105,20,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,179,1,101,1,10,2,109,8,10,1006,0,74,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,213,1006,0,60,2,1105,9,10,1,1005,11,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,1002,8,1,245,1,6,20,10,1,1103,11,10,2,6,11,10,2,1103,0,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1002,8,1,284,2,1103,12,10,2,1104,14,10,2,1004,12,10,2,1009,4,10,101,1,9,9,1007,9,968,10,1005,10,15,99,109,642,104,0,104,1,21102,1,48063419288,1,21102,1,337,0,1105,1,441,21101,0,846927340300,1,21101,0,348,0,1105,1,441,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,1,235245104151,1,21102,395,1,0,1105,1,441,21102,29032123584,1,1,21101,0,406,0,1105,1,441,3,10,104,0,104,0,3,10,104,0,104,0,21101,0,709047878500,1,21101,429,0,0,1106,0,441,21101,868402070284,0,1,21102,1,440,0,1105,1,441,99,109,2,22102,1,-1,1,21101,40,0,2,21101,0,472,3,21102,462,1,0,1105,1,505,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,467,468,483,4,0,1001,467,1,467,108,4,467,10,1006,10,499,1102,1,0,467,109,-2,2106,0,0,0,109,4,2101,0,-1,504,1207,-3,0,10,1006,10,522,21101,0,0,-3,22101,0,-3,1,21202,-2,1,2,21101,1,0,3,21102,541,1,0,1106,0,546,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,569,2207,-4,-2,10,1006,10,569,21202,-4,1,-4,1105,1,637,22102,1,-4,1,21201,-3,-1,2,21202,-2,2,3,21101,588,0,0,1105,1,546,22102,1,1,-4,21101,0,1,-1,2207,-4,-2,10,1006,10,607,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,629,21201,-1,0,1,21102,629,1,0,106,0,504,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0
"""
        |> Advent.removeNewlinesAtEnds


main : Program () ( Output1, Output2 ) Never
main =
    Advent.program
        { input = input_
        , parse1 = parse1
        , parse2 = parse2
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }
