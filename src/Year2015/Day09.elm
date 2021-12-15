module Year2015.Day09 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)
import List.Extra



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List ( ( String, String ), Int )


type alias Input2 =
    List ( ( String, String ), Int )


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> List.map parseLine


parseLine : String -> ( ( String, String ), Int )
parseLine string =
    case String.words string of
        [ c1, _, c2, _, distance ] ->
            ( ( c1, c2 ), Advent.unsafeToInt distance )

        _ ->
            Debug.todo "wrong input 1"


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    let
        cities : List String
        cities =
            input
                |> List.concatMap (\( ( c1, c2 ), _ ) -> [ c1, c2 ])
                |> List.Extra.unique

        reverseInput : List ( ( String, String ), Int )
        reverseInput =
            input
                |> List.map (\( ( c1, c2 ), dist ) -> ( ( c2, c1 ), dist ))

        distances : Dict ( String, String ) Int
        distances =
            Dict.fromList (input ++ reverseInput)

        totalDistance : List String -> Int
        totalDistance route =
            route
                |> List.Extra.groupsOfWithStep 2 1
                |> List.map
                    (\step ->
                        case step of
                            [ a, b ] ->
                                Advent.unsafeMaybe (Dict.get ( a, b ) distances)

                            _ ->
                                Debug.todo "wrong input 2"
                    )
                |> List.sum
    in
    List.Extra.permutations cities
        |> List.map totalDistance
        |> List.minimum
        |> Advent.unsafeMaybe


compute2 : Input2 -> Output2
compute2 input =
    let
        cities : List String
        cities =
            input
                |> List.concatMap (\( ( c1, c2 ), _ ) -> [ c1, c2 ])
                |> List.Extra.unique

        reverseInput : List ( ( String, String ), Int )
        reverseInput =
            input
                |> List.map (\( ( c1, c2 ), dist ) -> ( ( c2, c1 ), dist ))

        distances : Dict ( String, String ) Int
        distances =
            Dict.fromList (input ++ reverseInput)

        totalDistance : List String -> Int
        totalDistance route =
            route
                |> List.Extra.groupsOfWithStep 2 1
                |> List.map
                    (\step ->
                        case step of
                            [ a, b ] ->
                                Advent.unsafeMaybe (Dict.get ( a, b ) distances)

                            _ ->
                                Debug.todo "wrong input 2"
                    )
                |> List.sum
    in
    List.Extra.permutations cities
        |> List.map totalDistance
        |> List.maximum
        |> Advent.unsafeMaybe



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
Faerun to Norrath = 129
Faerun to Tristram = 58
Faerun to AlphaCentauri = 13
Faerun to Arbre = 24
Faerun to Snowdin = 60
Faerun to Tambi = 71
Faerun to Straylight = 67
Norrath to Tristram = 142
Norrath to AlphaCentauri = 15
Norrath to Arbre = 135
Norrath to Snowdin = 75
Norrath to Tambi = 82
Norrath to Straylight = 54
Tristram to AlphaCentauri = 118
Tristram to Arbre = 122
Tristram to Snowdin = 103
Tristram to Tambi = 49
Tristram to Straylight = 97
AlphaCentauri to Arbre = 116
AlphaCentauri to Snowdin = 12
AlphaCentauri to Tambi = 18
AlphaCentauri to Straylight = 91
Arbre to Snowdin = 129
Arbre to Tambi = 53
Arbre to Straylight = 40
Snowdin to Tambi = 15
Snowdin to Straylight = 99
Tambi to Straylight = 70
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
