module Year2017.Day23 exposing (..)

import Array.Hamt as Array exposing (Array)
import Advent exposing (Test)
import Dict exposing (Dict)


-- Today's part 2 is done largely by hand (and Python).
-- If you want a hint, see `startingCpu2` in this file.
-- If you want the solution, see the `Year2017/Day23.py` file.


main : Program Never ( Output, Output ) Never
main =
    Advent.program
        { input = input
        , parse1 = parse
        , parse2 = parse
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }


type alias Input =
    Array Instruction


type Instruction
    = Set Reg Content
    | Sub Reg Content
    | Mul Reg Content
    | Jnz Content Content


type alias Reg =
    String


type Content
    = R Reg
    | V Int


type alias Output =
    Int


parse : String -> Input
parse input =
    input
        |> String.lines
        |> List.map parseInstruction
        |> Array.fromList


parseInstruction : String -> Instruction
parseInstruction string =
    case String.words string of
        [ "set", x, y ] ->
            Set x (parseContent y)

        [ "sub", x, y ] ->
            Sub x (parseContent y)

        [ "mul", x, y ] ->
            Mul x (parseContent y)

        [ "jnz", x, y ] ->
            Jnz (parseContent x) (parseContent y)

        _ ->
            Debug.crash "wrong input!"


parseContent : String -> Content
parseContent string =
    case String.toInt string of
        Ok int ->
            V int

        Err _ ->
            R string


type alias CPU =
    Dict Reg Int


startingCpu : CPU
startingCpu =
    "abcdefgh"
        |> String.toList
        |> List.map String.fromChar
        |> List.map (\x -> ( x, 0 ))
        |> Dict.fromList


type alias Position =
    Int


type alias MulCounter =
    Int


compute1 : Input -> Output
compute1 instructions =
    --run instructions startingCpu 0 0
    --    |> Tuple.second
    -1


breakpoints : List Position
breakpoints =
    [ {- 4,8,10,11, -} 12, 14, 16, 20, 15, 25 ]


run : Array Instruction -> CPU -> Position -> MulCounter -> ( CPU, MulCounter )
run instructions cpu position counter =
    let
        _ =
            --if breakpoints |> List.member position then
            Debug.log (toString position) cpu

        --else
        --    cpu
    in
        case instructions |> Array.get position of
            Nothing ->
                ( cpu, counter )

            Just instruction ->
                case instruction of
                    Set x y ->
                        let
                            newCpu : CPU
                            newCpu =
                                cpu
                                    |> Dict.insert x (value y cpu)
                        in
                            run instructions newCpu (position + 1) counter

                    Sub x y ->
                        let
                            newCpu : CPU
                            newCpu =
                                cpu
                                    |> Dict.get x
                                    |> Advent.unsafeMaybe
                                    |> (\currentX -> currentX - value y cpu)
                                    |> (\newX -> Dict.insert x newX cpu)
                        in
                            run instructions newCpu (position + 1) counter

                    Mul x y ->
                        let
                            newCpu : CPU
                            newCpu =
                                cpu
                                    |> Dict.get x
                                    |> Advent.unsafeMaybe
                                    |> (\currentX -> currentX * value y cpu)
                                    |> (\newX -> Dict.insert x newX cpu)
                        in
                            run instructions newCpu (position + 1) (counter + 1)

                    Jnz x y ->
                        case value x cpu of
                            0 ->
                                run instructions cpu (position + 1) counter

                            _ ->
                                run instructions cpu (position + value y cpu) counter


value : Content -> CPU -> Int
value content cpu =
    case content of
        R reg ->
            cpu |> Dict.get reg |> Advent.unsafeMaybe

        V val ->
            val


startingCpu2 : CPU
startingCpu2 =
    startingCpu
        |> Dict.insert "a" 1
        -- debug
        |> Dict.insert "b" 125100
        |> Dict.insert "c" 125100
        |> Dict.insert "d" 125099
        |> Dict.insert "e" 125099
        |> Dict.insert "h" 1000


compute2 : Input -> Output
compute2 instructions =
    run instructions startingCpu2 11 0
        |> Tuple.first
        |> Dict.get "h"
        |> Advent.unsafeMaybe


tests1 : List (Test Input Output)
tests1 =
    []


tests2 : List (Test Input Output)
tests2 =
    []


input : String
input =
    """set b 81
set c b
jnz a 2
jnz 1 5
mul b 100
sub b -100000
set c b
sub c -17000
set f 1
set d 2
set e 2
set g d
mul g e
sub g b
jnz g 2
set f 0
sub e -1
set g e
sub g b
jnz g -8
sub d -1
set g d
sub g b
jnz g -13
jnz f 2
sub h -1
set g b
sub g c
jnz g 2
jnz 1 3
sub b -17
jnz 1 -23"""
