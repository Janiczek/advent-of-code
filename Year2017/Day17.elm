module Year2017.Day17 exposing (..)

import Advent exposing (Test)
import Array.Hamt as Array exposing (Array)


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
    Int


type alias Output =
    Int


steps : Int
steps =
    2017



-- insert AFTER the value you stop on


initial : Array Int
initial =
    [ 0 ]
        |> Array.fromList


parse : String -> Input
parse input =
    Advent.toInt input


compute1 : Input -> Output
compute1 input =
    List.range 1 2017
        |> List.foldl (spinAndAdd input) ( initial, 0 )
        |> valueAfter2017


spinAndAdd : Int -> Int -> ( Array Int, Int ) -> ( Array Int, Int )
spinAndAdd moveTimes current ( array, index ) =
    let
        length =
            Array.length array

        indexAfterMoving =
            (index + moveTimes) % length

        indexAfterInserting =
            indexAfterMoving + 1

        newArray =
            Array.slice 0 (indexAfterMoving + 1) array
                |> Array.push current
                |> (flip Array.append) (Array.slice (indexAfterMoving + 1) length array)
    in
        ( newArray, indexAfterInserting )


valueAfter2017 : ( Array Int, Int ) -> Int
valueAfter2017 ( array, index ) =
    array
        |> Array.get ((index + 1) % (Array.length array))
        |> Advent.unsafeMaybe


compute2 : Input -> Output
compute2 input =
    recurse input 1 -1 0


end : Int
end =
    50000000


recurse : Int -> Int -> Int -> Int -> Int
recurse moveTimes current valueAtIndex1 index =
    if current > end then
        valueAtIndex1
    else
        let
            length =
                current

            indexAfterMoving =
                (index + moveTimes) % length

            indexAfterInserting =
                indexAfterMoving + 1

            newValueAtIndex1 =
                if indexAfterInserting == 1 then
                    current
                else
                    valueAtIndex1
        in
            recurse moveTimes (current + 1) newValueAtIndex1 indexAfterInserting


tests1 : List (Test Input Output)
tests1 =
    [ Test "example" "3" 3 638 ]


tests2 : List (Test Input Output)
tests2 =
    []


input : String
input =
    "345"
