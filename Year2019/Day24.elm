module Year2019.Day24 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Set exposing (Set)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    Set ( Int, Int )


type alias Input2 =
    Set ( Int, Int )


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> List.indexedMap
            (\y line ->
                line
                    |> String.toList
                    |> List.indexedMap
                        (\x char ->
                            if char == '.' then
                                Nothing

                            else
                                Just ( x, y )
                        )
            )
        |> List.concat
        |> List.filterMap identity
        |> Set.fromList


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


tick : Set ( Int, Int ) -> Set ( Int, Int )
tick alive =
    List.range 0 4
        |> List.concatMap
            (\x ->
                List.range 0 4
                    |> List.map (\y -> ( x, y ))
            )
        |> List.filterMap
            (\coord ->
                case aliveCount coord alive of
                    1 ->
                        Just coord

                    2 ->
                        if Set.member coord alive then
                            Nothing

                        else
                            Just coord

                    _ ->
                        Nothing
            )
        |> Set.fromList


aliveCount : ( Int, Int ) -> Set ( Int, Int ) -> Int
aliveCount ( x, y ) alive =
    neighbours ( x, y )
        |> Set.intersect alive
        |> Set.size


neighbours : ( Int, Int ) -> Set ( Int, Int )
neighbours ( x, y ) =
    [ if x == 0 then
        Nothing

      else
        Just ( x - 1, y )
    , if x == 4 then
        Nothing

      else
        Just ( x + 1, y )
    , if y == 0 then
        Nothing

      else
        Just ( x, y - 1 )
    , if y == 4 then
        Nothing

      else
        Just ( x, y + 1 )
    ]
        |> List.filterMap identity
        |> Set.fromList


toScore : Set ( Int, Int ) -> Int
toScore alive =
    alive
        |> Set.toList
        |> List.map (\( x, y ) -> 2 ^ (5 * y + x))
        |> List.sum


compute1 : Input1 -> Output1
compute1 input =
    ( Set.empty, input )
        |> go1


go1 : ( Set Int, Set ( Int, Int ) ) -> Int
go1 ( scores, alive ) =
    let
        score =
            toScore alive
    in
    if Set.member score scores then
        score

    else
        go1 ( Set.insert score scores, tick alive )


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
##.#.
#.###
##...
...#.
#.##.
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
