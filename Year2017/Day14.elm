module Year2017.Day14 exposing (..)

import Advent exposing (Test)
import Year2017.Day10 as Knot
import Graph exposing (NodeContext, Node, Graph)
import IntDict
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
    String


type alias Output =
    Int


parse : String -> Input
parse input =
    input


gridOfHexes : Input -> List String
gridOfHexes input =
    List.range 0 127
        |> List.map (\i -> input ++ "-" ++ toString i)
        |> List.map Knot.hash


compute1 : Input -> Output
compute1 input =
    input
        |> gridOfHexes
        --|> List.map hashesAndDots
        --|> String.join "\n"
        --|> Debug.log "?"
        --|> Debug.crash "..."
        |> List.map count
        |> List.sum


hashesAndDots : String -> String
hashesAndDots hash =
    hash
        |> String.toList
        |> List.map charToHashesAndDots
        |> String.join ""


charToHashesAndDots : Char -> String
charToHashesAndDots char =
    case char of
        '0' ->
            "...."

        '1' ->
            "...#"

        '2' ->
            "..#."

        '3' ->
            "..##"

        '4' ->
            ".#.."

        '5' ->
            ".#.#"

        '6' ->
            ".##."

        '7' ->
            ".###"

        '8' ->
            "#..."

        '9' ->
            "#..#"

        'a' ->
            "#.#."

        'b' ->
            "#.##"

        'c' ->
            "##.."

        'd' ->
            "##.#"

        'e' ->
            "###."

        'f' ->
            "####"

        _ ->
            Debug.crash "wrong???"


bools : String -> List Bool
bools hash =
    hash
        |> String.toList
        |> List.concatMap charToBools


charToBools : Char -> List Bool
charToBools char =
    case char of
        '0' ->
            [ False, False, False, False ]

        '1' ->
            [ False, False, False, True ]

        '2' ->
            [ False, False, True, False ]

        '3' ->
            [ False, False, True, True ]

        '4' ->
            [ False, True, False, False ]

        '5' ->
            [ False, True, False, True ]

        '6' ->
            [ False, True, True, False ]

        '7' ->
            [ False, True, True, True ]

        '8' ->
            [ True, False, False, False ]

        '9' ->
            [ True, False, False, True ]

        'a' ->
            [ True, False, True, False ]

        'b' ->
            [ True, False, True, True ]

        'c' ->
            [ True, True, False, False ]

        'd' ->
            [ True, True, False, True ]

        'e' ->
            [ True, True, True, False ]

        'f' ->
            [ True, True, True, True ]

        _ ->
            Debug.crash "wrong???"


count : String -> Int
count hash =
    hash
        |> String.toList
        |> List.map countChar
        |> List.sum


countChar : Char -> Int
countChar char =
    case char of
        '0' ->
            0

        '1' ->
            1

        '2' ->
            1

        '3' ->
            2

        '4' ->
            1

        '5' ->
            2

        '6' ->
            2

        '7' ->
            3

        '8' ->
            1

        '9' ->
            2

        'a' ->
            2

        'b' ->
            3

        'c' ->
            2

        'd' ->
            3

        'e' ->
            3

        'f' ->
            4

        _ ->
            Debug.crash "wrong???"


findAnswer2 : List (List Bool) -> Int
findAnswer2 listOfLists =
    let
        grid =
            listOfLists
                |> List.map Array.fromList
                |> Array.fromList
    in
        grid
            |> Array.indexedMap
                (\y row ->
                    row
                        |> Array.indexedMap (,)
                        |> Array.filter (\( x, cell ) -> cell == True)
                        |> Array.map
                            (\( x, cell ) ->
                                makeNodeContext grid ( x, y ) cell
                            )
                )
            |> concatArrays
            |> Array.foldl Graph.insert Graph.empty
            |> numberOfConnectedRegions


compute2 : Input -> Output
compute2 input =
    --[ [ True, False, False ]
    --, [ True, True, False ]
    --, [ False, True, False ]
    --]
    input
        |> gridOfHexes
        |> List.map bools
        |> findAnswer2


concatArrays : Array (Array a) -> Array a
concatArrays arrays =
    arrays
        |> Array.foldl Array.append Array.empty


makeNodeContext : Grid -> ( Int, Int ) -> Bool -> NodeContext Bool ()
makeNodeContext grid ( x, y ) bool =
    let
        length =
            Array.length grid

        left =
            if x /= 0 && get grid (x - 1) y then
                Just (id length (x - 1) y)
            else
                Nothing

        right =
            if x /= (length - 1) && get grid (x + 1) y then
                Just (id length (x + 1) y)
            else
                Nothing

        top =
            if y /= 0 && get grid x (y - 1) then
                Just (id length x (y - 1))
            else
                Nothing

        bottom =
            if y /= (length - 1) && get grid x (y + 1) then
                Just (id length x (y + 1))
            else
                Nothing

        edges =
            [ left
            , right
            , top
            , bottom
            ]
                |> List.filterMap identity
                |> List.map (\x -> ( x, () ))
                |> IntDict.fromList
    in
        { node = Node (id length x y) bool
        , incoming = edges
        , outgoing = edges
        }


id : Int -> Int -> Int -> Int
id length x y =
    x * length + y


type alias Grid =
    Array (Array Bool)


get : Grid -> Int -> Int -> Bool
get grid x y =
    grid
        |> unsafeArrayGet y
        |> unsafeArrayGet x


unsafeArrayGet : Int -> Array a -> a
unsafeArrayGet index array =
    array
        |> Array.get index
        |> Advent.unsafeMaybe


numberOfConnectedRegions : Graph Bool () -> Int
numberOfConnectedRegions graph =
    case Graph.stronglyConnectedComponents graph of
        Err components ->
            List.length components

        Ok _ ->
            Debug.crash "number of connected regions"


tests1 : List (Test Input Output)
tests1 =
    [ Test "example"
        "flqrgnkx"
        "flqrgnkx"
        8108
    ]


tests2 : List (Test Input Output)
tests2 =
    [ Test "example"
        "flqrgnkx"
        "flqrgnkx"
        1242
    ]


input : String
input =
    "hwlqcszp"
