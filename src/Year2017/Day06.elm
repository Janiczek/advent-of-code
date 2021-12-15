module Year2017.Day06 exposing (..)

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
    Array Int


type alias Output =
    Int


input : String
input =
    "0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11"


parse : String -> Input
parse input =
    input
        |> String.split " "
        |> List.map Advent.toInt
        |> Array.fromList


compute1 : Input -> Output
compute1 input =
    -- TODO sets?
    process input []


process : Array Int -> List (Array Int) -> Int
process input memory =
    let
        newState =
            redistribute input
    in
        if memory |> List.member newState then
            List.length memory + 1
        else
            process newState (newState :: memory)


redistribute : Array Int -> Array Int
redistribute input =
    let
        ( maxIndex, max ) =
            maximum input

        inputWithoutMax =
            input
                |> Array.set maxIndex 0
    in
        redistributeHelper inputWithoutMax max (maxIndex + 1)


redistributeHelper : Array Int -> Int -> Index -> Array Int
redistributeHelper array toGive index =
    if index >= Array.length array then
        redistributeHelper array toGive 0
    else if toGive > 0 then
        let
            newArray =
                array
                    |> update index (\x -> x + 1)
        in
            redistributeHelper newArray (toGive - 1) (index + 1)
    else
        array


update : Index -> (a -> a) -> Array a -> Array a
update index fn array =
    array
        |> Array.get index
        |> Maybe.map
            (\old ->
                let
                    new =
                        fn old
                in
                    array
                        |> Array.set index new
            )
        |> Maybe.withDefault array


type alias Index =
    Int


maximum : Array Int -> ( Index, Int )
maximum array =
    array
        |> Array.indexedMap (,)
        |> Array.foldl
            (\( i, n ) ( mi, mn ) ->
                if n > mn then
                    ( i, n )
                else
                    ( mi, mn )
            )
            ( -1, -1 )


compute2 : Input -> Output
compute2 input =
    process2 input [ input ]


process2 : Array Int -> List (Array Int) -> Int
process2 input memory =
    let
        newState =
            redistribute input
    in
        if memory |> List.member newState then
            let
                startIndex =
                    memory
                        |> List.reverse
                        |> List.indexedMap (,)
                        |> List.filter (\( i, state ) -> state == newState)
                        |> List.head
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault -1

                endIndex =
                    List.length memory
            in
                endIndex - startIndex
        else
            process2 newState (newState :: memory)


tests1 : List (Test Input Output)
tests1 =
    [ Test "example"
        "0 2 7 0"
        ([ 0, 2, 7, 0 ] |> Array.fromList)
        5
    ]


tests2 : List (Test Input Output)
tests2 =
    [ Test "example"
        "0 2 7 0"
        ([ 0, 2, 7, 0 ] |> Array.fromList)
        4
    ]
