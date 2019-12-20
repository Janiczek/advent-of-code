module Year2019.Day17 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)
import Dict.Extra
import Fifo
import List.Extra
import Set exposing (Set)
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


type alias State =
    { cells : Dict Coord Char
    , computer : Result Stop Computer
    , roughPath : List ( Turn, Int )
    }


type Direction
    = Up
    | Down
    | Right
    | Left


type Turn
    = TurnRight
    | TurnLeft


initState1 : Memory -> State
initState1 mem =
    { cells = Dict.empty
    , computer = Ok <| Intcode.initWithMemory mem
    , roughPath = []
    }


initState2 : Memory -> State
initState2 mem =
    { cells = Dict.empty
    , computer =
        mem
            |> Memory.setMany [ ( 0, 2 ) ]
            |> Intcode.initWithMemory
            |> Ok
    , roughPath = []
    }


type alias Coord =
    ( Int, Int )


compute1 : Input1 -> Output1
compute1 mem =
    let
        state =
            initState1 mem
                |> run
                |> outputsToCells

        _ =
            print "look" state.cells
    in
    state
        |> findIntersections
        |> List.map alignmentParameter
        |> List.sum


print : String -> Dict Coord Char -> ()
print log cells =
    let
        _ =
            cells
                |> Dict.toList
                |> Dict.Extra.groupBy (Tuple.first >> Tuple.second)
                |> Dict.toList
                |> List.sortBy (Tuple.first >> negate)
                |> List.map (Tuple.mapSecond (List.sortBy (Tuple.first >> Tuple.first)))
                |> List.map
                    (Tuple.second
                        >> List.map Tuple.second
                        >> String.fromList
                        >> Debug.log log
                    )
    in
    ()


alignmentParameter : Coord -> Int
alignmentParameter ( x, y ) =
    x * y


neighbours : Coord -> List Coord
neighbours ( x, y ) =
    [ ( x - 1, y )
    , ( x + 1, y )
    , ( x, y - 1 )
    , ( x, y + 1 )
    ]


move : Int -> Direction -> Coord -> Coord
move n direction ( x, y ) =
    case direction of
        -- TODO are up and down switched?
        Up ->
            ( x, y - n )

        Down ->
            ( x, y + n )

        Left ->
            ( x - n, y )

        Right ->
            ( x + n, y )


turnRight : Direction -> Direction
turnRight direction =
    case direction of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


turnLeft : Direction -> Direction
turnLeft direction =
    case direction of
        Up ->
            Left

        Left ->
            Down

        Down ->
            Right

        Right ->
            Up


findIntersections : State -> List Coord
findIntersections { cells } =
    cells
        |> Dict.filter (\_ cell -> cell == '#')
        |> Dict.keys
        |> List.filter
            (\cell ->
                neighbours cell
                    |> List.filterMap (\neighbour -> Dict.get neighbour cells)
                    |> List.all ((==) '#')
            )


run : State -> State
run state =
    case state.computer of
        Ok computer ->
            { state | computer = Intcode.stepUntilStopped computer }

        Err (Halted computer) ->
            Debug.todo "halted - wat"

        Err (UnknownOpcode op computer) ->
            Debug.todo "unknown op - wat"

        Err (WaitsForInput computer) ->
            Debug.todo "waits for input - maybe correct"


outputsToCells : State -> State
outputsToCells state =
    let
        ( outputs, computerAfterOutputs ) =
            Intcode.getOutputs state.computer

        cells =
            outputs
                |> List.map Char.fromCode
                |> String.fromList
                |> String.trimRight
                |> String.lines
                |> List.map String.toList
                |> List.indexedMap
                    (\y row ->
                        List.indexedMap
                            (\x cell ->
                                ( ( x, y ), cell )
                            )
                            row
                    )
                |> List.concat
                |> Dict.fromList
    in
    { state
        | computer = computerAfterOutputs
        , cells = cells
    }


findRoughPath : State -> State
findRoughPath state =
    let
        currentPosition : Coord
        currentPosition =
            state.cells
                |> Dict.Extra.find (\_ cell -> cell == '^')
                |> Advent.unsafeMaybe "current position"
                |> Tuple.first

        comingFrom : Coord
        comingFrom =
            currentPosition
                |> Tuple.mapSecond (\y -> y - 1)

        roughPath : List ( Turn, Int )
        roughPath =
            findRoughPathHelp [] comingFrom currentPosition Up state.cells

        _ =
            roughPath
                |> roughPathToAscii
                |> Debug.log "rough path"
    in
    { state | roughPath = roughPath }


roughPathToAscii : List ( Turn, Int ) -> String
roughPathToAscii turns =
    turns
        |> List.map
            (\( turn_, amount ) ->
                (case turn_ of
                    TurnLeft ->
                        "L"

                    TurnRight ->
                        "R"
                )
                    ++ ","
                    ++ String.fromInt amount
            )
        |> String.join ","


findRoughPathHelp : List ( Turn, Int ) -> Coord -> Coord -> Direction -> Dict Coord Char -> List ( Turn, Int )
findRoughPathHelp soFar comingFrom current currentDirection cells =
    if not <| Dict.Extra.any (\_ cell -> cell == '#') cells then
        List.reverse soFar

    else
        let
            clockwiseDirection =
                turnRight currentDirection

            rightCoord =
                move 1 clockwiseDirection current

            pathExistsToRight =
                Dict.get rightCoord cells == Just '#'

            newDirection =
                if pathExistsToRight then
                    clockwiseDirection

                else
                    turnLeft currentDirection

            newTurn =
                turn currentDirection newDirection

            amount =
                findAmount current newDirection cells 0

            newComingFrom =
                current

            newCurrent =
                move amount newDirection current

            newSoFar =
                ( newTurn, amount ) :: soFar

            newCells =
                cells
                    |> Dict.map
                        (\coord cell ->
                            if cell == '#' && isInPath coord current newDirection amount then
                                'O'

                            else
                                cell
                        )
        in
        findRoughPathHelp newSoFar newComingFrom newCurrent newDirection newCells


findAmount : Coord -> Direction -> Dict Coord Char -> Int -> Int
findAmount current direction cells soFar =
    let
        cell =
            Dict.get current cells
    in
    if cell /= Just '.' && cell /= Nothing then
        findAmount (move 1 direction current) direction cells (soFar + 1)

    else
        soFar - 1


isInPath : Coord -> Coord -> Direction -> Int -> Bool
isInPath ( x, y ) ( fromX, fromY ) direction amount =
    case direction of
        Up ->
            fromX == x && y <= fromY && y >= (fromY - amount)

        Down ->
            fromX == x && y >= fromY && y <= (fromY + amount)

        Left ->
            fromY == y && x <= fromX && x >= (fromX - amount)

        Right ->
            fromY == y && x >= fromX && x <= (fromX + amount)


turn : Direction -> Direction -> Turn
turn current next =
    case ( current, next ) of
        ( Up, Left ) ->
            TurnLeft

        ( Up, Right ) ->
            TurnRight

        ( Right, Up ) ->
            TurnLeft

        ( Right, Down ) ->
            TurnRight

        ( Down, Right ) ->
            TurnLeft

        ( Down, Left ) ->
            TurnRight

        ( Left, Down ) ->
            TurnLeft

        ( Left, Up ) ->
            TurnRight

        _ ->
            Debug.todo "90° turn can't be made"


goStraight : Coord -> Coord -> Direction -> Dict Coord Char -> ( Turn, Int )
goStraight comingFrom current currentDirection cells =
    let
        direction =
            Debug.todo "Direction"

        steps =
            Debug.todo "Steps"
    in
    ( direction, steps )


compute2 : Input2 -> Output2
compute2 mem =
    let
        _ =
            initState2 mem
                |> run
                |> outputsToCells
                |> findRoughPath
    in
    mem
        |> runProgram
            """A,B,A,C,B,C,B,A,C,B
L,6,R,8,R,12,L,6,L,8
L,10,L,8,R,12
L,8,L,10,L,6,L,6
"""
        |> List.reverse
        |> List.head
        |> Advent.unsafeMaybe "compute2"


runProgram : String -> Memory -> List Int
runProgram program mem =
    mem
        |> Memory.setMany [ ( 0, 2 ) ]
        |> Intcode.initWithMemory
        |> addStringInput program
        |> addStringInput "n\n"
        |> Intcode.stepUntilStopped
        |> Intcode.getOutputs
        |> Tuple.first


addStringInput : String -> Computer -> Computer
addStringInput string computer =
    string
        |> String.toList
        |> List.foldl
            (\char c ->
                Intcode.addInput
                    (Char.toCode char)
                    c
            )
            computer



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example"
                """..#..........
        ..#..........
        #######...###
        #.#...#...#.#
        #############
        ..#...#...#..
        ..#####...^.."""
                Nothing
                -- Just "parsed-input"
                76
     -}
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
1,330,331,332,109,3508,1102,1,1182,16,1101,0,1473,24,101,0,0,570,1006,570,36,1002,571,1,0,1001,570,-1,570,1001,24,1,24,1105,1,18,1008,571,0,571,1001,16,1,16,1008,16,1473,570,1006,570,14,21102,58,1,0,1105,1,786,1006,332,62,99,21102,1,333,1,21101,0,73,0,1106,0,579,1102,0,1,572,1101,0,0,573,3,574,101,1,573,573,1007,574,65,570,1005,570,151,107,67,574,570,1005,570,151,1001,574,-64,574,1002,574,-1,574,1001,572,1,572,1007,572,11,570,1006,570,165,101,1182,572,127,1002,574,1,0,3,574,101,1,573,573,1008,574,10,570,1005,570,189,1008,574,44,570,1006,570,158,1105,1,81,21102,340,1,1,1106,0,177,21101,477,0,1,1105,1,177,21102,514,1,1,21102,1,176,0,1106,0,579,99,21101,184,0,0,1106,0,579,4,574,104,10,99,1007,573,22,570,1006,570,165,101,0,572,1182,21102,375,1,1,21101,211,0,0,1105,1,579,21101,1182,11,1,21102,222,1,0,1105,1,979,21102,388,1,1,21102,233,1,0,1105,1,579,21101,1182,22,1,21101,0,244,0,1106,0,979,21102,401,1,1,21101,255,0,0,1106,0,579,21101,1182,33,1,21101,266,0,0,1106,0,979,21101,0,414,1,21102,1,277,0,1106,0,579,3,575,1008,575,89,570,1008,575,121,575,1,575,570,575,3,574,1008,574,10,570,1006,570,291,104,10,21101,0,1182,1,21101,0,313,0,1106,0,622,1005,575,327,1102,1,1,575,21102,1,327,0,1106,0,786,4,438,99,0,1,1,6,77,97,105,110,58,10,33,10,69,120,112,101,99,116,101,100,32,102,117,110,99,116,105,111,110,32,110,97,109,101,32,98,117,116,32,103,111,116,58,32,0,12,70,117,110,99,116,105,111,110,32,65,58,10,12,70,117,110,99,116,105,111,110,32,66,58,10,12,70,117,110,99,116,105,111,110,32,67,58,10,23,67,111,110,116,105,110,117,111,117,115,32,118,105,100,101,111,32,102,101,101,100,63,10,0,37,10,69,120,112,101,99,116,101,100,32,82,44,32,76,44,32,111,114,32,100,105,115,116,97,110,99,101,32,98,117,116,32,103,111,116,58,32,36,10,69,120,112,101,99,116,101,100,32,99,111,109,109,97,32,111,114,32,110,101,119,108,105,110,101,32,98,117,116,32,103,111,116,58,32,43,10,68,101,102,105,110,105,116,105,111,110,115,32,109,97,121,32,98,101,32,97,116,32,109,111,115,116,32,50,48,32,99,104,97,114,97,99,116,101,114,115,33,10,94,62,118,60,0,1,0,-1,-1,0,1,0,0,0,0,0,0,1,42,14,0,109,4,1201,-3,0,586,21001,0,0,-1,22101,1,-3,-3,21102,0,1,-2,2208,-2,-1,570,1005,570,617,2201,-3,-2,609,4,0,21201,-2,1,-2,1106,0,597,109,-4,2105,1,0,109,5,1202,-4,1,630,20101,0,0,-2,22101,1,-4,-4,21101,0,0,-3,2208,-3,-2,570,1005,570,781,2201,-4,-3,652,21001,0,0,-1,1208,-1,-4,570,1005,570,709,1208,-1,-5,570,1005,570,734,1207,-1,0,570,1005,570,759,1206,-1,774,1001,578,562,684,1,0,576,576,1001,578,566,692,1,0,577,577,21102,702,1,0,1105,1,786,21201,-1,-1,-1,1105,1,676,1001,578,1,578,1008,578,4,570,1006,570,724,1001,578,-4,578,21102,1,731,0,1105,1,786,1105,1,774,1001,578,-1,578,1008,578,-1,570,1006,570,749,1001,578,4,578,21101,0,756,0,1106,0,786,1106,0,774,21202,-1,-11,1,22101,1182,1,1,21102,774,1,0,1106,0,622,21201,-3,1,-3,1106,0,640,109,-5,2106,0,0,109,7,1005,575,802,21002,576,1,-6,21001,577,0,-5,1106,0,814,21101,0,0,-1,21101,0,0,-5,21101,0,0,-6,20208,-6,576,-2,208,-5,577,570,22002,570,-2,-2,21202,-5,55,-3,22201,-6,-3,-3,22101,1473,-3,-3,2102,1,-3,843,1005,0,863,21202,-2,42,-4,22101,46,-4,-4,1206,-2,924,21102,1,1,-1,1105,1,924,1205,-2,873,21101,35,0,-4,1105,1,924,1201,-3,0,878,1008,0,1,570,1006,570,916,1001,374,1,374,1201,-3,0,895,1102,1,2,0,2101,0,-3,902,1001,438,0,438,2202,-6,-5,570,1,570,374,570,1,570,438,438,1001,578,558,922,20102,1,0,-4,1006,575,959,204,-4,22101,1,-6,-6,1208,-6,55,570,1006,570,814,104,10,22101,1,-5,-5,1208,-5,37,570,1006,570,810,104,10,1206,-1,974,99,1206,-1,974,1102,1,1,575,21101,973,0,0,1106,0,786,99,109,-7,2105,1,0,109,6,21101,0,0,-4,21102,0,1,-3,203,-2,22101,1,-3,-3,21208,-2,82,-1,1205,-1,1030,21208,-2,76,-1,1205,-1,1037,21207,-2,48,-1,1205,-1,1124,22107,57,-2,-1,1205,-1,1124,21201,-2,-48,-2,1106,0,1041,21102,1,-4,-2,1105,1,1041,21102,1,-5,-2,21201,-4,1,-4,21207,-4,11,-1,1206,-1,1138,2201,-5,-4,1059,1201,-2,0,0,203,-2,22101,1,-3,-3,21207,-2,48,-1,1205,-1,1107,22107,57,-2,-1,1205,-1,1107,21201,-2,-48,-2,2201,-5,-4,1090,20102,10,0,-1,22201,-2,-1,-2,2201,-5,-4,1103,2101,0,-2,0,1106,0,1060,21208,-2,10,-1,1205,-1,1162,21208,-2,44,-1,1206,-1,1131,1106,0,989,21101,439,0,1,1105,1,1150,21101,477,0,1,1106,0,1150,21102,514,1,1,21102,1149,1,0,1106,0,579,99,21102,1157,1,0,1105,1,579,204,-2,104,10,99,21207,-3,22,-1,1206,-1,1138,1201,-5,0,1176,2102,1,-4,0,109,-6,2106,0,0,40,9,46,1,7,1,46,1,7,1,46,1,7,1,46,1,7,1,46,1,7,1,42,13,42,1,3,1,50,1,3,1,50,1,3,1,50,1,3,9,42,1,11,1,36,9,9,1,36,1,5,1,1,1,9,1,18,13,5,1,5,7,5,1,18,1,17,1,7,1,9,1,8,7,3,1,13,11,1,1,9,1,8,1,5,1,3,1,13,1,3,1,5,1,1,1,9,1,6,7,1,1,3,1,13,1,3,1,5,1,1,1,9,1,6,1,1,1,3,1,1,1,3,1,13,1,3,1,5,1,1,1,9,1,6,1,1,1,3,1,1,1,3,1,13,1,3,1,5,1,1,1,9,1,6,1,1,1,3,1,1,1,3,1,13,1,3,1,5,1,1,1,9,1,6,1,1,11,13,1,3,7,1,1,9,8,5,1,1,1,17,1,11,1,15,2,5,1,1,1,5,13,11,9,7,2,5,1,1,1,5,1,31,1,7,10,5,1,31,1,7,1,6,1,7,1,31,1,7,1,6,1,7,1,25,11,3,1,6,1,7,1,25,1,5,1,3,1,3,1,6,9,25,1,1,13,40,1,1,1,3,1,3,1,44,1,1,1,3,1,3,1,44,1,1,1,3,1,3,1,44,7,3,1,46,1,7,1,46,9,4
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
