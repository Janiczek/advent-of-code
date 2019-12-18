module Year2019.Day18 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

{- TODO: this takes too long for example 4 (as the upper bound for number of
   possible paths is n!)

   We could trim the paths taken by remembering the best score and maze for a
   given subset of collected keys... ie. if we've previously collected keys `a`
   and `b` (no need to worry about the order), the cost is X, and we now get into
   situation where we've collected the same set of keys (using a different path)
   and the cost is higher, there's (probably) no need continuing in the current
   path and we can use the previous one.

   This might backfire, as it doesn't take the current position into account.
   Maybe we should make the "primary key" (heh) not only the set of the collected
   keys, but also the position? so [a, b, c] and pos (X,Y) is equivalent to
   [b, a, c] and pos (X,Y), and the one with the smaller cost wins?

   That seems equivalent to ([a,b,c], c) - the set of keys and the last key
   collected...

   Anyway, we'll need to switch to some kind of BFS queue system instead of this
   naive non-information-sharing DFS.

-}

import AStar exposing (Path)
import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Char
import Dict exposing (Dict)
import Dict.Extra
import List.Extra
import Set exposing (Set)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    Maze


type alias Input2 =
    Maze


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


type alias Coord =
    ( Int, Int )


type Object
    = Wall
    | Door Char
    | Key Char
    | Floor


type alias Maze =
    { objects : Dict Coord Object
    , keys : Dict Char Coord
    , position : Coord
    , cost : Int
    }


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
                            ( x, y, char )
                        )
            )
        |> List.concat
        |> List.foldl
            (\( x, y, char ) maze ->
                let
                    newPosition =
                        if char == '@' then
                            ( x, y )

                        else
                            maze.position

                    newKeys =
                        if Char.isLower char then
                            Dict.insert char ( x, y ) maze.keys

                        else
                            maze.keys

                    newObjects =
                        Dict.insert ( x, y ) (parseObject char) maze.objects
                in
                { maze
                    | objects = newObjects
                    , position = newPosition
                    , keys = newKeys
                }
            )
            { objects = Dict.empty
            , keys = Dict.empty
            , position = ( -1, -1 )
            , cost = 0
            }


parseObject : Char -> Object
parseObject char =
    case char of
        '#' ->
            Wall

        '@' ->
            Floor

        '.' ->
            Floor

        _ ->
            if Char.isLower char then
                Key char

            else if Char.isUpper char then
                Door char

            else
                Debug.todo "parseObject wat"


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Maze -> Output1
compute1 maze =
    let
        pathsToKeys : List Path
        pathsToKeys =
            maze.keys
                |> Dict.values
                |> List.filterMap (pathToKey maze)
    in
    if List.isEmpty pathsToKeys then
        maze.cost
            |> Debug.log "finished with"

    else
        pathsToKeys
            |> List.map (usePath maze >> compute1)
            |> List.minimum
            |> Advent.unsafeMaybe "compute1"


usePath : Maze -> Path -> Maze
usePath maze path =
    let
        newPosition =
            path
                |> List.reverse
                |> List.head
                |> Advent.unsafeMaybe "newPosition"

        key =
            maze.objects
                |> Dict.get newPosition
                |> Advent.unsafeMaybe "key"
                |> keyToChar
    in
    { maze
        | position = newPosition
        , cost = maze.cost + List.length path
        , keys = Dict.remove key maze.keys
        , objects =
            maze.objects
                |> Dict.insert newPosition Floor
                |> Dict.map
                    (\_ object ->
                        if object == Door (Char.toUpper key) then
                            Floor

                        else
                            object
                    )
    }


keyToChar : Object -> Char
keyToChar object =
    case object of
        Key c ->
            c

        _ ->
            Debug.todo "wat keyToChar"


pathToKey : Maze -> Coord -> Maybe Path
pathToKey maze keyCoord =
    --let
    --    _ =
    --        print ("finding " ++ Debug.toString keyCoord) maze
    --in
    AStar.findPath
        AStar.straightLineCost
        (movesFrom maze.objects)
        maze.position
        keyCoord


print : String -> Maze -> ()
print log maze =
    let
        _ =
            maze.objects
                |> Dict.toList
                |> Dict.Extra.groupBy (Tuple.first >> Tuple.second)
                |> Dict.toList
                |> List.sortBy (Tuple.first >> negate)
                |> List.map (Tuple.mapSecond (List.sortBy (Tuple.first >> Tuple.first)))
                |> List.map
                    (Tuple.second
                        >> List.map
                            (\( ( x, y ), obj ) ->
                                case obj of
                                    Floor ->
                                        if maze.position == ( x, y ) then
                                            '@'

                                        else
                                            '.'

                                    Wall ->
                                        '#'

                                    Key k ->
                                        k

                                    Door k ->
                                        Char.toUpper k
                            )
                        >> String.fromList
                        >> Debug.log log
                    )
    in
    ()


movesFrom : Dict Coord Object -> Coord -> Set Coord
movesFrom objects ( x, y ) =
    [ ( x - 1, y )
    , ( x + 1, y )
    , ( x, y - 1 )
    , ( x, y + 1 )
    ]
        |> Set.fromList
        |> Set.filter
            (\coord ->
                case Dict.get coord objects of
                    Just Floor ->
                        True

                    Just (Key _) ->
                        True

                    _ ->
                        False
            )


compute2 : Input2 -> Output2
compute2 input =
    -1



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "ex 1"
        """#########
#b.A.@.a#
#########"""
        Nothing
        -- Just "parsed-input"
        8
    , Test "ex 2"
        """########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################"""
        Nothing
        -- Just "parsed-input"
        86
    , Test "ex 3"
        """########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################"""
        Nothing
        -- Just "parsed-input"
        132
    , Test "ex 4"
        """#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################"""
        Nothing
        -- Just "parsed-input"
        136
    , Test "ex 5"
        """########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################"""
        Nothing
        -- Just "parsed-input"
        81
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
#################################################################################
#.........#.......#...#...#..v..#.....#.#.........#...#....g#...L...#......z#...#
#.#.#####.#.#####.###.#.#.#.#.###I#.#.#.#.#.#######E#.#.###.#.#####.#.#####.#.###
#.#.....#...#.........#.#...#.....#.#...#.#.#..n....#.#.#.#...#...#.#...#...#..p#
#.#####.#####.#######.#.###########.###.#.#.#.#.#####.#.#.#####.#.#.###.#O###.#.#
#.#...#.#.....#...#...#...........#.#...#.#.#.#.#...#.#.#.......#.#.....#.#...#.#
#.###.#.#######.#.#.###########.###.#####.#.#.###.#.#.#.#.#####.#########.#####.#
#...#.#..y..#...#.#...#.........#...#...#.#.#.....#.#...#.#...#.........#.#.....#
###.#.#####.#.###.#####.#########.###.#.#.#.#######.#####.#.#.###.#.#####.#.#####
#...#....c#...#.#.......#.......#.#...#.#.#.......#....x#.#.#.#...#.#.....#.....#
#.###.#.#######.#########.#####.#.#.###.#.#####.#.#####.###.#.#.#####.#####.###.#
#.#...#.................#.....#.#.....#.#...#...#...#.#...#.#.#.#.....#...#.#...#
#.#.#############.#.#####.###.#.#.#####.#####.#####.#.###.#.#.#.#.#####.#.###.#.#
#.#...#.....#...#.#.#...#...#.#.#.#.....#.....#...#.....#.#.#...#.....#.#.#...#.#
#.###.#.#.#.#.###.###.#.###.#.#.#.#.###.#.###.#.#.#####.#.#.#########.#.#.#.###.#
#...#.#.#.#k..#...#...#...#.#.#.#.#...#.#.#...#.#.#.....#.#.........#.#.#...#.#.#
###.#.###F#####.###.#####.###.#.#####.#.#.#####.#.#.#####.#.#####.###.#.#####.#.#
#.#.#...#...#...#...#...#.....#.....#.#.#.......#.#.....#.#.....#.....#.#.....#.#
#.#.###.###.#.###.#.#.#.###########.#.###.#######.#####.#.#######.#####.#.#####.#
#.P.#.#.....#.#...#.#.#.....#...#...#...#...#...#.#...#.#.....H.#...#...#.....#.#
#.###.#.#####.#.###.#.#####.#.###.#####.#.###.#.#.#.#.#########.###.###.#.###.#.#
#...#...#.....#.#...#.#.....#.....#.....#.#...#...#.#.......#.#.#.#.#...#.#.#...#
###.#####.#####.#######.#####.#####.#.#.#.#.#######.#####.#.#.#.#.#.#.###.#.#####
#.#.#...#.....#.....#...#...#...#.#.#.#.#.#...#.....#.....#.#...#.#.#...#...#...#
#.#.#.#.#####.#####.#.###.#.###.#.#.#.###.###.#.#####.#.#########.#.###U#####.#.#
#.#...#...#...#.....#.#.#.#...#.#...#...#...#...#.#...#.#.....#.....#...#.....#.#
#.#######.#.#.#.#####.#.#.###.#.#.#####.###.#####.#.#####.###.#.#####.###.#####.#
#.....#...#.#.#...#...#...#.#...#...#...#.#.#...#.#.....#.#.#.#.#.....#...#.....#
#.#####.###.#.###.#.###.###.#####.###W#.#.#.#.#.#.#####.#.#.#.#.#.#.###.###.###.#
#...#...#...#.#.#...#.#.....#.....#...#.#.#.#.#.......#...#.#...#.#...#.#.#...#.#
#.#.#.###.###.#.#####.#####.#####.#.#####.#.#.#######.#####.#######.#.#T#.###.#.#
#.#.#.#.....#.....#.....#...#...#.#.....#...#.#.....#.#...........#.#.....#...#.#
#.#.#.#.###.#####.#.#.###.###.#.#.#####.#.###.#.###.###.#####.###.#########.###.#
#.#...#...#...#.#.#.#.#...#...#.#.#.....#...#.....#.......#...#.#.#.....#...#...#
#.#######.###.#.#.#.###.###.###.###.###.###.#############.#.###.#.#.###.#.###.###
#.#.......#.#.#.....#...#...J.#.....#...#.#...#...#...#...#...#...#...#...#...#.#
#.#######.#.#.#######.#######.#######.###.###.#.#.#.#.#######.#.###.#.#####.###.#
#.......#...#.#.......#.....#.....#.#.#.#...#...#...#...#.....#...#.#...#.#...#.#
#######.#####.#.#######.###.#####.#.#.#.#.#############.#.#######.#####.#.###.#.#
#.............#.........#.........#.......................#.............#.......#
#######################################.@.#######################################
#.........#...#.........#.....#...#...........#...........#..q....#.....#.......#
#####.###.#.###D#####.###.#.#.#.###.#.#.#.###.#######.#.#.#####.#.###.#.#.#####.#
#.....#.....#...#.#...#...#.#...#...#.#.#...#.......#.#.#j#...#.#.#...#d#.....#.#
#.#.#######.#.###.#.#.#.###.###.#.###.#.#.#.#######.###.#.#.#.#.#.#.###.###.#.###
#t#.#.....#.#...#.#.#.#...#...#.#.#.#.#.#.#...#....w....#...#...#...#.#...#.#...#
#.###.###.#.###.#.#.#####.###.###.#.#.#.#####.#######.###############.###.#.###.#
#.....#.#.#.#...#.#...Q.#...#.......#.#.#.....#...#...#.......#.#.....#...#...#.#
#.#####.#.###.###.#####.###.#######.#.###.###.#.#.#####.#####.#.#.###.#.###.###.#
#.#.....#...#.#.......#.....#...#...#...#.#...#.#.#.......#...#...#.#...#...#...#
#.#####.###.#.#R#####.#######.#.#######.#.#####.#.#.#######.###.###.#######.#.###
#.....#...#...#.#...#...#.....#.....#...#...#...#...#.......#.........#...#.#...#
#####.#.#.#######.#.#.#.#.#########.#.#.###.#.#######.###############.#.#.#.###.#
#.#...#.#...#.....#.#.#.#...#...#...#.#.#.#...#...#...#.............#...#.#...#.#
#.#.###.###.#S#####.#.#####.#.###.###.#.#.#####.#.#.###.###########.#####.#####.#
#...#...#.#...#...#...#.....#...#.#...#.#....m#.#.#...#.......#.....#...#.....#.#
#.#####.#.#######.###.#.#####.#.#.###.#.#.###.#.#.###.###.#####.#####.#.#####.#.#
#u......#.........#...#...#.#.#.#...#.#.#.#...#.#...#.#...#.....#.....#.....#.#.#
#########.#########.#####.#.#.#.###.#.#.#.#####.###.#.#.###.#######.#####.###.#.#
#.M.....#.......#...#.....#.#.#...#.#.#.#.........#.#.#.#.#.......#..s#.#.#...#.#
#####.#.#.#####.#.###.#####.#.###.#.#.###.#######.#.#.#.#.#######.###.#.#.#.###.#
#.....#.#.....#.#...#.#.....#.#.#...#...#...#...#.#.#...#...#.....#.#.#.#...#...#
#.#########B###.###.#######.#.#.#######.#####.#.###.#######.#######.#.#.#####.#.#
#.#.A...#...#.....#.....#...#.#.#.....#.#.....#...#...#.......#.....#.#.......#.#
#.#.###.#.###.#########.#.###.#.#.#.#.#.#.#######.#.#.#.#####.#.###.#.#######.#.#
#.....#.#...#.........#.#.#...#...#.#...#.....#.....#...#a....#.#...#.....#...#.#
#####.#.#.###########.#.#.#.###.###.###.#.###.#.###############.#.#.#####.#.###.#
#...#.#...#...........#...#...#...#.#...#.#...#.#.......#.......#.#.#.....#.#...#
#.#.#######.#############.###.#####.#####.#.###.#.#####.#.#######.###.#####.#.###
#.#.........#.............#.#.V...#.....#.#.C.#.#.....#.#.#.....#...#.#r....#...#
#.#########.#.#############.#####.#####.#####.#.#####.#.#.#####.###.#.#.#######.#
#.#.........#...#...#.G...#.....#.#...#.#...#.#.#.#...#...#b..#.#...#.Y.#.....#.#
#.#############.#.#.#####.#.#.###.#.#.#.#.#.#.#.#.#.#######.#.#.#.#######.#.###.#
#.#...........#...#.....#...#.#...#.#.#.#.#...#...#.#...K...#...#.#.......#.#...#
#.#X#######.###########.#####.#.###.#.#.#.#########.#####.#######.#.#.#######.###
#.#...#...#...N.#.#...#.#l..#.#...#.#...#.......#...#..f#.......#...#.#......o#.#
#.###.#.#.###.#.#.#.#.#.#.#.#.###.#.###.#.#####.#.###.#.#.#####.#####.#.#######.#
#.#...#.#.....#...#.#.#...#...#...#..i#.#...#.#...#...#.#.#.....#.#.Z.#...#...#.#
#.#.###.###########.#.#######.#.#####.#.###.#.#######.#.###.###.#.#.#####.#.#.#.#
#h....#..........e..#.........#.......#.#.............#.....#.....#.........#...#
#################################################################################
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
