module Year2017.Day21 exposing (..)

import Advent exposing (Test)
import List.Extra
import EveryDict as Dict exposing (EveryDict)


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


type alias Grid =
    List (List Bool)


type alias Input =
    -- rules for 2, rules for 3
    EveryDict Grid Grid


type alias Output =
    Int


startingGrid : Grid
startingGrid =
    [ [ False, True, False ]
    , [ False, False, True ]
    , [ True, True, True ]
    ]


parse : String -> Input
parse input =
    let
        ruleStrings =
            String.lines input

        rules =
            List.map parseRule ruleStrings

        rotatedRules =
            rules
                |> List.concatMap
                    (\( input, output ) ->
                        input
                            |> rotations
                            |> List.map (\rotation -> ( rotation, output ))
                    )
    in
        rotatedRules
            |> Dict.fromList


parseRule : String -> ( Grid, Grid )
parseRule string =
    case String.split " => " string of
        [ input, output ] ->
            ( parseGrid input
            , parseGrid output
            )

        _ ->
            Debug.crash "wrong input!"


parseGrid : String -> Grid
parseGrid string =
    string
        |> String.split "/"
        |> List.map parseRow


parseRow : String -> List Bool
parseRow string =
    string
        |> String.toList
        |> List.map
            (\c ->
                case c of
                    '.' ->
                        False

                    '#' ->
                        True

                    _ ->
                        Debug.crash "wrong input!"
            )


type alias IterationsLeft =
    Int


compute1 : EveryDict Grid Grid -> Output
compute1 rules =
    run 5 rules startingGrid


run : IterationsLeft -> EveryDict Grid Grid -> Grid -> Int
run iterationsLeft rules grid =
    if (Debug.log "new run" iterationsLeft) == 0 then
        countTrue grid
    else
        let
            length =
                List.length (visualize "run starts with" grid)

            isDivisibleBy2 =
                length % 2 == 0

            groupSize =
                if isDivisibleBy2 then
                    2
                else
                    3

            newGroupSize =
                if isDivisibleBy2 then
                    3
                else
                    4

            numberOfGroupsInRow : Int
            numberOfGroupsInRow =
                length // groupSize

            oneSetOfIndexes : List Int
            oneSetOfIndexes =
                -- [0,1]
                List.range 0 (numberOfGroupsInRow - 1)
                    -- [0,2]
                    |> List.map ((*) groupSize)

            indexesForTopLeftCorners : List ( Int, Int )
            indexesForTopLeftCorners =
                -- [(0,0),(0,2),(2,0),(2,2)]
                oneSetOfIndexes
                    |> List.Extra.andThen
                        (\x ->
                            oneSetOfIndexes
                                |> List.Extra.andThen (\y -> [ ( x, y ) ])
                        )

            --|> Debug.log "indexes for top left corners"
            groups : List Grid
            groups =
                -- TEST
                grid
                    |> makeGroups indexesForTopLeftCorners groupSize
                    |> List.map (visualize "groups")

            groupsAfterMatching : List Grid
            groupsAfterMatching =
                groups
                    |> List.map (matchRules rules)
                    |> List.map (visualize "groups after matching")

            newGrid : Grid
            newGrid =
                groupsAfterMatching
                    |> List.Extra.groupsOf numberOfGroupsInRow
                    |> List.Extra.transpose
                    |> List.concatMap
                        (\rowOfGrids ->
                            -- [    [[1,2,3]   ,  [[a,b,c]
                            --      ,[4,5,6]]     ,[d,e,f]]   ]
                            -- --------->
                            -- [[1,2,3,a,b,c]
                            -- ,[4,5,6,d,e,f]]
                            (List.foldl
                                (\rightGrid leftGrid ->
                                    List.map2
                                        (++)
                                        leftGrid
                                        rightGrid
                                )
                                (List.repeat newGroupSize [])
                                rowOfGrids
                            )
                                |> visualize "row after append"
                        )
                    |> visualize "new grid output"
        in
            run (iterationsLeft - 1) rules newGrid


compressed : Grid -> String
compressed grid =
    grid
        |> List.map
            (\row ->
                row
                    |> List.map
                        (\bool ->
                            if bool then
                                '#'
                            else
                                '.'
                        )
                    |> String.fromList
            )
        |> String.join "/"


visualizeRule : String -> ( Grid, Grid ) -> ( Grid, Grid )
visualizeRule description (( input, output ) as rule) =
    --let
    --    string =
    --        compressed input ++ " => " ++ compressed output
    --    _ =
    --        Debug.log string description
    --in
    rule


visualize : String -> Grid -> Grid
visualize description grid =
    --let
    --    string =
    --        "\n"
    --            ++ (grid
    --                    |> List.map
    --                        (\row ->
    --                            row
    --                                |> List.map
    --                                    (\bool ->
    --                                        if bool then
    --                                            '#'
    --                                        else
    --                                            '.'
    --                                    )
    --                                |> String.fromList
    --                        )
    --                    |> String.join "\n"
    --               )
    --    _ =
    --        Debug.log string description
    --in
    grid


makeGroups : List ( Int, Int ) -> Int -> Grid -> List Grid
makeGroups indexes groupSize grid =
    {-
       [[1,2,3,4]
       ,[a,b,c,d]
       ,[z,y,x,w]
       ,[5,6,7,8]
       ]

       [ [[1,2]   [[3,4]
         ,[a,b]]  ,[c,d]]

         [[z,y]   [[x,w]
         ,[5,6]]   [7,8]] ]
    -}
    indexes
        |> List.map (makeGroup groupSize grid)


makeGroup : Int -> Grid -> ( Int, Int ) -> Grid
makeGroup groupSize grid ( x, y ) =
    grid
        |> List.drop y
        |> List.take groupSize
        |> List.map (List.drop x)
        |> List.map (List.take groupSize)


matchRules : EveryDict Grid Grid -> Grid -> Grid
matchRules rules grid =
    case rules |> Dict.get grid of
        Nothing ->
            Debug.crash "no rule matched"

        Just output ->
            output


rotations : Grid -> List Grid
rotations grid =
    [ grid
    , grid |> rotateCw
    , grid |> rotateCw |> rotateCw
    , grid |> rotateCw |> rotateCw |> rotateCw
    , grid |> flip
    , grid |> rotateCw |> flip
    , grid |> rotateCw |> rotateCw |> flip
    , grid |> rotateCw |> rotateCw |> rotateCw |> flip
    ]


rotateCw : Grid -> Grid
rotateCw grid =
    grid
        |> List.Extra.transpose
        |> flip


flip : Grid -> Grid
flip grid =
    -- vertically, doesn't matter
    grid
        |> List.reverse


countTrue : Grid -> Int
countTrue grid =
    grid
        |> List.map
            (\row ->
                row
                    |> List.filter identity
                    |> List.length
            )
        |> List.sum


compute2 : EveryDict Grid Grid -> Output
compute2 rules =
    run 18 rules startingGrid


tests1 : List (Test Input Output)
tests1 =
    []


tests2 : List (Test Input Output)
tests2 =
    []


input : String
input =
    """../.. => .../.../###
#./.. => .../.#./.##
##/.. => .#./.#./...
.#/#. => ###/..#/.##
##/#. => ..#/###/#..
##/## => ..#/#../##.
.../.../... => .##./##../..##/.##.
#../.../... => ##../.#.#/..#./###.
.#./.../... => ##.#/#.#./.#../..##
##./.../... => ...#/##.#/.#.#/#.##
#.#/.../... => ..#./#.../###./...#
###/.../... => #.#./...#/#.#./###.
.#./#../... => ...#/###./.##./...#
##./#../... => ###./####/###./..##
..#/#../... => ####/#.../####/#.##
#.#/#../... => #.##/.#.#/##.#/###.
.##/#../... => ..../.#../.#.#/.##.
###/#../... => ..##/##.#/..##/.###
.../.#./... => ###./..##/.#../#..#
#../.#./... => ###./.#../#.../#...
.#./.#./... => ####/..#./.##./##..
##./.#./... => .#../#.#./###./###.
#.#/.#./... => ####/.##./##.#/.###
###/.#./... => #.#./..##/.##./#...
.#./##./... => ####/#.##/####/..#.
##./##./... => #.../.#../..../#.##
..#/##./... => #..#/..##/#.../####
#.#/##./... => ###./##../..##/#...
.##/##./... => ..../#.##/.###/#.#.
###/##./... => .#../##.#/.#../##..
.../#.#/... => ...#/.###/.##./###.
#../#.#/... => ###./##../#.#./.##.
.#./#.#/... => ..#./.#../.##./.###
##./#.#/... => #.../#.../.##./.#..
#.#/#.#/... => .##./..##/.###/#...
###/#.#/... => ..../####/###./....
.../###/... => #.##/.#.#/#.##/...#
#../###/... => #.../#.#./.#../#...
.#./###/... => ...#/###./.##./.#.#
##./###/... => ##../####/###./#.##
#.#/###/... => ...#/###./##.#/.#.#
###/###/... => #.#./##.#/..../.##.
..#/.../#.. => ...#/..#./..#./##..
#.#/.../#.. => ..#./#.##/#.#./#.##
.##/.../#.. => ####/####/#.##/#...
###/.../#.. => ###./..#./###./.#..
.##/#../#.. => ...#/####/..../###.
###/#../#.. => ##.#/.#../##.#/...#
..#/.#./#.. => ###./#.##/...#/##..
#.#/.#./#.. => #.../..#./..#./#.##
.##/.#./#.. => ##.#/...#/#.#./.###
###/.#./#.. => .#../..##/#.#./..#.
.##/##./#.. => #.../#.#./.###/#...
###/##./#.. => .##./.#../.#.#/.###
#../..#/#.. => ###./#..#/#.../##.#
.#./..#/#.. => #.#./#..#/#.../.###
##./..#/#.. => ...#/..##/..#./####
#.#/..#/#.. => ####/#..#/###./#.#.
.##/..#/#.. => ..#./..#./..../.##.
###/..#/#.. => ...#/#..#/#.#./....
#../#.#/#.. => ..##/.#.#/.###/.##.
.#./#.#/#.. => ..../##.#/..##/#..#
##./#.#/#.. => ..#./..##/#..#/#..#
..#/#.#/#.. => ..#./#.../#.#./##..
#.#/#.#/#.. => ##.#/..##/.###/...#
.##/#.#/#.. => #.##/.##./##../#.#.
###/#.#/#.. => ####/##.#/#..#/#.#.
#../.##/#.. => ..##/#.#./####/####
.#./.##/#.. => ##../###./####/....
##./.##/#.. => .###/####/..#./...#
#.#/.##/#.. => ###./##../##../#.##
.##/.##/#.. => ##../.###/####/.#.#
###/.##/#.. => ##../.##./#.../..#.
#../###/#.. => #.#./.#.#/#.../....
.#./###/#.. => .##./##../...#/##..
##./###/#.. => #.#./..../.##./##.#
..#/###/#.. => ...#/...#/##.#/...#
#.#/###/#.. => .##./.###/#..#/.##.
.##/###/#.. => ####/..##/#.../####
###/###/#.. => ...#/####/..#./.###
.#./#.#/.#. => .##./#.##/.##./.###
##./#.#/.#. => ..##/.#../##.#/###.
#.#/#.#/.#. => .#../..../.#.#/#...
###/#.#/.#. => ###./..#./..../#.#.
.#./###/.#. => #..#/.#../#.../..##
##./###/.#. => .##./...#/.###/....
#.#/###/.#. => .###/###./#.#./.#.#
###/###/.#. => #.##/.#.#/#.#./.##.
#.#/..#/##. => .###/..../####/####
###/..#/##. => #.##/###./..##/.##.
.##/#.#/##. => ..../...#/#..#/..##
###/#.#/##. => #.##/.#../.#../....
#.#/.##/##. => ..##/..##/#.../#..#
###/.##/##. => ##.#/#.../#.##/..##
.##/###/##. => ...#/..#./##../#.##
###/###/##. => #.##/#..#/..#./...#
#.#/.../#.# => ##.#/.#../##.#/.##.
###/.../#.# => #.#./..##/.#.#/##.#
###/#../#.# => ..#./#.##/...#/.###
#.#/.#./#.# => .###/#.##/#..#/#.##
###/.#./#.# => ..../..#./###./..#.
###/##./#.# => .###/##../..##/####
#.#/#.#/#.# => #.#./####/.#../.##.
###/#.#/#.# => ####/..../..##/#...
#.#/###/#.# => #.../.##./#.../...#
###/###/#.# => .#.#/...#/..../..##
###/#.#/### => .#../#.##/#.##/.###
###/###/### => #.../.#.#/#..#/#.##"""
