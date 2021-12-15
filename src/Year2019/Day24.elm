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
    Set ( Int, Int, Int )


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
                                Just ( x, y, 0 )
                        )
            )
        |> List.concat
        |> List.filterMap identity
        |> Set.fromList



-- 3. COMPUTE (actually solve the problem)


tick1 : Set ( Int, Int ) -> Set ( Int, Int )
tick1 alive =
    List.range 0 4
        |> List.concatMap
            (\x ->
                List.range 0 4
                    |> List.map (\y -> ( x, y ))
            )
        |> List.filterMap
            (\coord ->
                case aliveNeighbours1 coord alive of
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


aliveNeighbours1 : ( Int, Int ) -> Set ( Int, Int ) -> Int
aliveNeighbours1 coord alive =
    neighbours1 coord
        |> Set.intersect alive
        |> Set.size


neighbours1 : ( Int, Int ) -> Set ( Int, Int )
neighbours1 ( x, y ) =
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
        go1 ( Set.insert score scores, tick1 alive )


compute2 : Input2 -> Output2
compute2 input =
    go2 200 input


go2 : Int -> Set ( Int, Int, Int ) -> Int
go2 minutesLeft alive =
    if Debug.log "go2" minutesLeft <= 0 then
        Set.size alive

    else
        go2 (minutesLeft - 1) (tick2 alive)


tick2 : Set ( Int, Int, Int ) -> Set ( Int, Int, Int )
tick2 alive =
    let
        depths =
            alive
                |> Set.toList
                |> List.map (\( _, _, depth ) -> depth)

        minDepth =
            List.minimum depths
                |> Advent.unsafeMaybe "tick2 minDepth"

        maxDepth =
            List.maximum depths
                |> Advent.unsafeMaybe "tick2 maxDepth"
    in
    List.range (minDepth - 1) (maxDepth + 1)
        |> List.concatMap
            (\depth ->
                List.range 0 4
                    |> List.concatMap
                        (\x ->
                            List.range 0 4
                                |> List.map (\y -> ( x, y, depth ))
                        )
            )
        |> List.filterMap
            (\coord ->
                case aliveNeighbours2 coord alive of
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


aliveNeighbours2 : ( Int, Int, Int ) -> Set ( Int, Int, Int ) -> Int
aliveNeighbours2 coord alive =
    neighbours2 coord
        |> Set.intersect alive
        |> Set.size


neighbours2 : ( Int, Int, Int ) -> Set ( Int, Int, Int )
neighbours2 ( x, y, depth ) =
    if x == 2 && y == 2 then
        Set.empty

    else
        [ -- left
          if x == 0 then
            [ ( 1, 2, depth - 1 ) ]

          else if x == 3 && y == 2 then
            [ ( 4, 0, depth + 1 )
            , ( 4, 1, depth + 1 )
            , ( 4, 2, depth + 1 )
            , ( 4, 3, depth + 1 )
            , ( 4, 4, depth + 1 )
            ]

          else
            [ ( x - 1, y, depth ) ]
        , -- right
          if x == 4 then
            [ ( 3, 2, depth - 1 ) ]

          else if x == 1 && y == 2 then
            [ ( 0, 0, depth + 1 )
            , ( 0, 1, depth + 1 )
            , ( 0, 2, depth + 1 )
            , ( 0, 3, depth + 1 )
            , ( 0, 4, depth + 1 )
            ]

          else
            [ ( x + 1, y, depth ) ]
        , -- up
          if y == 0 then
            [ ( 2, 1, depth - 1 ) ]

          else if x == 2 && y == 3 then
            [ ( 0, 4, depth + 1 )
            , ( 1, 4, depth + 1 )
            , ( 2, 4, depth + 1 )
            , ( 3, 4, depth + 1 )
            , ( 4, 4, depth + 1 )
            ]

          else
            [ ( x, y - 1, depth ) ]
        , -- down
          if y == 4 then
            [ ( 2, 3, depth - 1 ) ]

          else if x == 2 && y == 1 then
            [ ( 0, 0, depth + 1 )
            , ( 1, 0, depth + 1 )
            , ( 2, 0, depth + 1 )
            , ( 3, 0, depth + 1 )
            , ( 4, 0, depth + 1 )
            ]

          else
            [ ( x, y + 1, depth ) ]
        ]
            |> List.concat
            |> Set.fromList



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
