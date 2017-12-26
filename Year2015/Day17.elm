module Year2015.Day17 exposing (..)

import Advent exposing (Test)
import List.Extra


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
    ( Int, List Container )


type alias Output =
    Int


parse : String -> Input
parse input =
    case String.lines input of
        liters :: rest ->
            ( Advent.toInt liters
            , List.indexedMap (\i str -> ( i, Advent.toInt str )) rest
            )

        _ ->
            Debug.crash "wrong input!"


type alias Container =
    ( Index, Size )


type alias Index =
    Int


type alias Size =
    Int


compute1 : Input -> Output
compute1 ( liters, containers ) =
    containers
        |> List.Extra.subsequences
        |> List.filter (\combination -> sum combination == liters)
        |> List.length


sum : List Container -> Int
sum containers =
    containers
        |> List.map Tuple.second
        |> List.sum


compute2 : Input -> Output
compute2 ( liters, containers ) =
    let
        goodWays =
            containers
                |> List.Extra.subsequences
                |> List.filter (\combination -> sum combination == liters)

        lengthOfMinimal =
            goodWays
                |> List.sortBy List.length
                |> List.head
                |> Advent.unsafeMaybe
                |> List.length

        minimalWays =
            goodWays
                |> List.filter (\way -> List.length way == lengthOfMinimal)
    in
        minimalWays
            |> List.length


tests1 : List (Test Input Output)
tests1 =
    [ Test "example"
        """25
20
15
10
5
5"""
        ( 25, [ ( 0, 20 ), ( 1, 15 ), ( 2, 10 ), ( 3, 5 ), ( 4, 5 ) ] )
        4
    ]


tests2 : List (Test Input Output)
tests2 =
    []


input : String
input =
    """150
33
14
18
20
45
35
16
35
1
13
18
13
50
44
48
6
24
41
30
42"""
