module Year2017.Day22 exposing (..)

import Advent exposing (Test)
import Dict exposing (Dict)


main : Program Never ( Output, Output ) Never
main =
    Advent.program
        { input = input
        , parse1 = parse1
        , parse2 = parse2
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }


type alias Input1 =
    -- position of the carrier
    ( Grid1, Position )


type alias Input2 =
    -- position of the carrier
    ( Grid2, Position )


type alias Output =
    Int


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Position =
    ( Int, Int )


type alias Carrier =
    { direction : Direction
    , position : Position
    }


type alias Grid1 =
    Dict Position NodeState1


type alias Grid2 =
    Dict Position NodeState2


type NodeState1
    = Infected
    | Clean


type NodeState2
    = Infected2
    | Clean2
    | Weakened
    | Flagged


startingDirection : Direction
startingDirection =
    Up


turn1 : NodeState1 -> Direction -> Direction
turn1 state direction =
    case state of
        Infected ->
            turnRight direction

        Clean ->
            turnLeft direction


turn2 : NodeState2 -> Direction -> Direction
turn2 state direction =
    case state of
        Infected2 ->
            turnRight direction

        Clean2 ->
            turnLeft direction

        Weakened ->
            direction

        Flagged ->
            turnLeft (turnLeft direction)


turnRight : Direction -> Direction
turnRight direction =
    case direction of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


turnLeft : Direction -> Direction
turnLeft direction =
    case direction of
        Up ->
            Left

        Left ->
            Down

        Down ->
            Right

        Right ->
            Up


parse1 : String -> Input1
parse1 input =
    let
        lines =
            input
                |> String.lines

        grid =
            lines
                -- (y, string)
                |> List.indexedMap (,)
                |> List.concatMap parseRow1
                |> Dict.fromList

        middle =
            -- 3 -> 1 ?
            List.length lines // 2

        startingPosition =
            ( middle, middle )
    in
        ( grid
        , startingPosition
        )


parse2 : String -> Input2
parse2 input =
    let
        lines =
            input
                |> String.lines

        grid =
            lines
                -- (y, string)
                |> List.indexedMap (,)
                |> List.concatMap parseRow2
                |> Dict.fromList

        middle =
            -- 3 -> 1 ?
            List.length lines // 2

        startingPosition =
            ( middle, middle )
    in
        ( grid
        , startingPosition
        )


parseRow1 : ( Int, String ) -> List ( Position, NodeState1 )
parseRow1 ( y, string ) =
    string
        |> String.toList
        -- (x, char)
        |> List.indexedMap (,)
        |> List.map (parseChar1 y)


parseRow2 : ( Int, String ) -> List ( Position, NodeState2 )
parseRow2 ( y, string ) =
    string
        |> String.toList
        -- (x, char)
        |> List.indexedMap (,)
        |> List.map (parseChar2 y)


parseChar1 : Int -> ( Int, Char ) -> ( Position, NodeState1 )
parseChar1 y ( x, char ) =
    ( ( x, y )
    , case char of
        '.' ->
            Clean

        '#' ->
            Infected

        _ ->
            Debug.crash "wrong input"
    )


parseChar2 : Int -> ( Int, Char ) -> ( Position, NodeState2 )
parseChar2 y ( x, char ) =
    ( ( x, y )
    , case char of
        '.' ->
            Clean2

        '#' ->
            Infected2

        _ ->
            Debug.crash "wrong input"
    )


compute1 : Input1 -> Output
compute1 ( grid, startingPosition ) =
    run1 10000 grid 0 (Carrier startingDirection startingPosition)


toggle1 : NodeState1 -> NodeState1
toggle1 state =
    case state of
        Clean ->
            Infected

        Infected ->
            Clean


toggle2 : NodeState2 -> NodeState2
toggle2 state =
    case state of
        Clean2 ->
            Weakened

        Infected2 ->
            Flagged

        Weakened ->
            Infected2

        Flagged ->
            Clean2


move : Direction -> Position -> Position
move direction ( x, y ) =
    case direction of
        Up ->
            ( x, y - 1 )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )


run1 : Int -> Grid1 -> Int -> Carrier -> Int
run1 bursts grid counter { position, direction } =
    if bursts == 0 then
        counter
    else
        let
            currentState =
                grid
                    |> Dict.get position
                    |> Maybe.withDefault Clean

            newDirection =
                turn1 currentState direction

            newState =
                toggle1 currentState

            newGrid =
                grid
                    |> Dict.insert position newState

            newPosition =
                move newDirection position

            newCounter =
                if newState == Infected then
                    counter + 1
                else
                    counter

            newCarrier =
                Carrier newDirection newPosition
        in
            run1 (bursts - 1) newGrid newCounter newCarrier


run2 : Int -> Grid2 -> Int -> Carrier -> Int
run2 bursts grid counter { position, direction } =
    if bursts == 0 then
        counter
    else
        let
            _ =
                if bursts % 100000 == 0 then
                    Debug.log "left" bursts
                else
                    bursts

            currentState =
                grid
                    |> Dict.get position
                    |> Maybe.withDefault Clean2

            newDirection =
                turn2 currentState direction

            newState =
                toggle2 currentState

            newGrid =
                grid
                    |> Dict.insert position newState

            newPosition =
                move newDirection position

            newCounter =
                if newState == Infected2 then
                    counter + 1
                else
                    counter

            newCarrier =
                Carrier newDirection newPosition
        in
            run2 (bursts - 1) newGrid newCounter newCarrier


compute2 : Input2 -> Output
compute2 ( grid, startingPosition ) =
    run2 10000000 grid 0 (Carrier startingDirection startingPosition)


tests1 : List (Test Input1 Output)
tests1 =
    [ Test "example"
        """..#
#..
..."""
        ( [ ( ( 0, 0 ), Clean )
          , ( ( 1, 0 ), Clean )
          , ( ( 2, 0 ), Infected )
          , ( ( 0, 1 ), Infected )
          , ( ( 1, 1 ), Clean )
          , ( ( 2, 1 ), Clean )
          , ( ( 0, 2 ), Clean )
          , ( ( 1, 2 ), Clean )
          , ( ( 2, 2 ), Clean )
          ]
            |> Dict.fromList
        , ( 1, 1 )
        )
        5587
    ]


tests2 : List (Test Input2 Output)
tests2 =
    []


input : String
input =
    """.########.....#...##.####
....#..#.#.##.###..#.##..
##.#.#..#.###.####.##.#..
####...#...####...#.##.##
..#...###.#####.....##.##
..#.##.######.#...###...#
.#....###..##....##...##.
##.##..####.#.######...##
#...#..##.....#..#...#..#
........#.##..###.#.....#
#.#..######.#.###..#...#.
.#.##.##..##.####.....##.
.....##..#....#####.#.#..
...#.#.#..####.#..###..#.
##.#..##..##....#####.#..
.#.#..##...#.#####....##.
.####.#.###.####...#####.
...#...######..#.##...#.#
#..######...#.####.#..#.#
...##..##.#.##.#.#.#....#
###..###.#..#.....#.##.##
..#....##...#..#..##..#..
.#.###.##.....#.###.#.###
####.##...#.#....#..##...
#.....#.#..#.##.#..###..#"""
