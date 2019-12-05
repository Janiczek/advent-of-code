module Year2019.Day02 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Array exposing (Array)
import Year2019.Intcode as Intcode
    exposing
        ( Mask(..)
        , Memory
        , Op(..)
        , Parameter
        )



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
    Intcode.parse string


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 mem =
    let
        memWith1202 =
            Intcode.init
                [ ( 1, 12 )
                , ( 2, 2 )
                ]
                mem
    in
    memWith1202
        |> go 0
        |> Intcode.get 0


go : Int -> Memory -> Memory
go position mem =
    case Intcode.step parseOpcode process position mem of
        Err err ->
            let
                _ =
                    Debug.log "Error" err
            in
            Debug.todo "Crashed when stepping an Intcode program"

        Ok ( Nothing, newMem ) ->
            -- we've HALTed
            newMem

        Ok ( Just newPosition, newMem ) ->
            go newPosition newMem


process : Op -> Int -> Memory -> ( Maybe Int, Memory )
process op position mem =
    case op of
        Add { addr0, addr1, dest } ->
            let
                num0 =
                    Intcode.getParam addr0 mem

                num1 =
                    Intcode.getParam addr1 mem

                newMem =
                    Intcode.setParam dest (num0 + num1) mem
            in
            ( Just (position + 4), newMem )

        Mult { addr0, addr1, dest } ->
            let
                num0 =
                    Intcode.getParam addr0 mem

                num1 =
                    Intcode.getParam addr1 mem

                newMem =
                    Intcode.setParam dest (num0 * num1) mem
            in
            ( Just (position + 4), newMem )

        Halt ->
            ( Nothing, mem )


parseOpcode : Int -> Memory -> Maybe Op
parseOpcode =
    Intcode.parseWith
        [ ( 1
          , Op3
                ( DontCare, DontCare, WantPosition )
                (\addr0 addr1 dest ->
                    Add
                        { addr0 = addr0
                        , addr1 = addr1
                        , dest = dest
                        }
                )
          )
        , ( 2
          , Op3
                ( DontCare, DontCare, WantPosition )
                (\addr0 addr1 dest ->
                    Mult
                        { addr0 = addr0
                        , addr1 = addr1
                        , dest = dest
                        }
                )
          )
        , ( 99, Op0 Halt )
        ]


type Op
    = Add { addr0 : Parameter, addr1 : Parameter, dest : Parameter }
    | Mult { addr0 : Parameter, addr1 : Parameter, dest : Parameter }
    | Halt


compute2 : Input2 -> Output2
compute2 mem =
    let
        ( noun, verb ) =
            findParameters 19690720 mem
    in
    100 * noun + verb


findParameters : Int -> Memory -> ( Int, Int )
findParameters wantedOutput mem =
    List.range 0 99
        |> List.concatMap (\noun -> List.map (Tuple.pair noun) (List.range 0 99))
        |> List.map (tryParameters mem)
        |> List.filter (\{ output } -> output == wantedOutput)
        |> List.head
        |> Advent.unsafeMaybe
        |> .inputs


tryParameters : Memory -> ( Int, Int ) -> { output : Int, inputs : ( Int, Int ) }
tryParameters mem ( noun, verb ) =
    let
        output =
            Intcode.init
                [ ( 1, noun )
                , ( 2, verb )
                ]
                mem
                |> go 0
                |> Intcode.get 0
    in
    { output = output, inputs = ( noun, verb ) }



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example"
        "input"
        -1
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
1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,6,23,27,1,27,5,31,2,9,31,35,1,5,35,39,2,6,39,43,2,6,43,47,1,5,47,51,2,9,51,55,1,5,55,59,1,10,59,63,1,63,6,67,1,9,67,71,1,71,6,75,1,75,13,79,2,79,13,83,2,9,83,87,1,87,5,91,1,9,91,95,2,10,95,99,1,5,99,103,1,103,9,107,1,13,107,111,2,111,10,115,1,115,5,119,2,13,119,123,1,9,123,127,1,5,127,131,2,131,6,135,1,135,5,139,1,139,6,143,1,143,6,147,1,2,147,151,1,151,5,0,99,2,14,0,0
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
