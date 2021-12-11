module Year2021.Day11 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Grid exposing (Grid)
import List.Extra as List
import Set exposing (Set)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    Grid Int


type alias Input2 =
    Grid Int


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> List.map (String.toList >> List.map Advent.digitCharToInt)
        |> Grid.from2DList (\_ -> 0)


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


logGrid : String -> Grid Int -> Grid Int
logGrid label grid =
    let
        data =
            grid
                |> Grid.toList
                |> List.map (Tuple.mapSecond String.fromInt)
                |> List.groupWhile (\( ( x1, _ ), _ ) ( ( x2, _ ), _ ) -> x1 == x2)
                |> List.map (\( x, xs ) -> x :: xs)
                |> List.transpose
                |> List.map (List.map Tuple.second >> String.concat)
                |> String.join "\n"

        _ =
            Debug.log (data ++ "\n            ") label
    in
    grid


step : ( Int, Grid Int, Int ) -> ( Int, Grid Int, Int )
step ( flashCount, grid, step_ ) =
    let
        gridAfterAdding =
            Grid.map (\_ v -> v + 1) grid

        ( newlyFlashedCount, gridAfterFlashing, flashedPositions ) =
            Advent.doUntil (==) flashGrid ( 0, gridAfterAdding, Set.empty )
    in
    let
        newGrid =
            gridAfterFlashing
                |> Grid.map
                    (\_ n ->
                        if n > 9 then
                            0

                        else
                            n
                    )
                |> logGrid ("after step " ++ String.fromInt step_)
    in
    ( flashCount + newlyFlashedCount, newGrid, step_ + 1 )


flashGrid : ( Int, Grid Int, Set ( Int, Int ) ) -> ( Int, Grid Int, Set ( Int, Int ) )
flashGrid ( flashCount, grid, flashedPositions ) =
    let
        newFlashedPositions =
            grid
                |> Grid.toList
                |> List.filterMap
                    (\( pos, n ) ->
                        if n > 9 then
                            Just pos

                        else
                            Nothing
                    )
                |> Set.fromList
                |> (\newF -> Set.diff newF flashedPositions)

        newGrid =
            Set.foldl flashPosition grid newFlashedPositions
    in
    ( flashCount + Set.size newFlashedPositions
    , newGrid
    , Set.union flashedPositions newFlashedPositions
    )


flashPosition : ( Int, Int ) -> Grid Int -> Grid Int
flashPosition pos grid =
    Grid.mapPositions
        (Grid.allNeighbourPositions pos)
        (\_ v -> v + 1)
        grid


compute1 : Input1 -> Output1
compute1 input =
    ( 0, logGrid "start" input, 1 )
        |> Advent.doNTimes 100 step
        |> (\( flashCount, _, _ ) -> flashCount)


compute2 : Input2 -> Output2
compute2 input =
    ( 0, logGrid "start" input, 1 )
        |> Advent.doUntil (\_ ( _, newGrid, _ ) -> Grid.all (\_ n -> n == 0) newGrid) step
        |> (\( _, _, stepCount ) -> stepCount - 1)



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "example"
        """5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"""
        Nothing
        1656
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    [ Test "example"
        """5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"""
        Nothing
        195
    ]



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
4525436417
1851242553
5421435521
8431325447
4517438332
3521262111
3331541734
4351836641
2753881442
7717616863
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
