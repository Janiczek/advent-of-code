module Year2019.Day19 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

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


type alias Coord =
    ( Int, Int )


type alias State1 =
    { mem : Memory
    , todo : List Coord
    , count : Int
    }


initState1 : Input1 -> State1
initState1 mem =
    { mem = mem
    , todo =
        List.range 0 49
            |> List.concatMap
                (\x ->
                    List.range 0 49
                        |> List.map (\y -> ( x, y ))
                )
    , count = 0
    }


compute1 : Input1 -> Output1
compute1 mem =
    initState1 mem
        |> go1


go1 : State1 -> Int
go1 state =
    case state.todo of
        [] ->
            state.count

        ( x, y ) :: rest ->
            let
                computer =
                    Intcode.initWithMemory state.mem
                        |> Intcode.addInput x
                        |> Intcode.addInput y
                        |> Intcode.stepUntilStopped

                ( outputs, newComputer ) =
                    computer
                        |> Intcode.getOutputs

                hit =
                    case outputs of
                        [ 1 ] ->
                            True

                        [ 0 ] ->
                            False

                        _ ->
                            Debug.todo "wat hit"

                newCount =
                    if hit then
                        state.count + 1

                    else
                        state.count
            in
            go1
                { state
                    | todo = rest
                    , count = newCount
                }


compute2 : Input2 -> Output2
compute2 input =
    -1



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
109,424,203,1,21102,1,11,0,1105,1,282,21102,18,1,0,1106,0,259,2102,1,1,221,203,1,21102,31,1,0,1105,1,282,21101,38,0,0,1106,0,259,21002,23,1,2,22101,0,1,3,21102,1,1,1,21101,57,0,0,1105,1,303,1202,1,1,222,20102,1,221,3,20102,1,221,2,21102,1,259,1,21102,80,1,0,1105,1,225,21102,72,1,2,21101,91,0,0,1105,1,303,1201,1,0,223,20102,1,222,4,21101,0,259,3,21102,1,225,2,21102,1,225,1,21102,1,118,0,1105,1,225,20102,1,222,3,21101,104,0,2,21101,0,133,0,1105,1,303,21202,1,-1,1,22001,223,1,1,21102,148,1,0,1106,0,259,1201,1,0,223,20101,0,221,4,20102,1,222,3,21101,0,18,2,1001,132,-2,224,1002,224,2,224,1001,224,3,224,1002,132,-1,132,1,224,132,224,21001,224,1,1,21101,195,0,0,106,0,109,20207,1,223,2,20101,0,23,1,21102,1,-1,3,21102,214,1,0,1106,0,303,22101,1,1,1,204,1,99,0,0,0,0,109,5,2102,1,-4,249,22102,1,-3,1,22102,1,-2,2,22102,1,-1,3,21101,250,0,0,1105,1,225,22102,1,1,-4,109,-5,2106,0,0,109,3,22107,0,-2,-1,21202,-1,2,-1,21201,-1,-1,-1,22202,-1,-2,-2,109,-3,2105,1,0,109,3,21207,-2,0,-1,1206,-1,294,104,0,99,21202,-2,1,-2,109,-3,2105,1,0,109,5,22207,-3,-4,-1,1206,-1,346,22201,-4,-3,-4,21202,-3,-1,-1,22201,-4,-1,2,21202,2,-1,-1,22201,-4,-1,1,21202,-2,1,3,21101,0,343,0,1105,1,303,1105,1,415,22207,-2,-3,-1,1206,-1,387,22201,-3,-2,-3,21202,-2,-1,-1,22201,-3,-1,3,21202,3,-1,-1,22201,-3,-1,2,21201,-4,0,1,21102,384,1,0,1106,0,303,1105,1,415,21202,-4,-1,-4,22201,-4,-3,-4,22202,-3,-2,-2,22202,-2,-4,-4,22202,-3,-2,-3,21202,-4,-1,-2,22201,-3,-2,1,21202,1,1,-4,109,-5,2105,1,0
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
