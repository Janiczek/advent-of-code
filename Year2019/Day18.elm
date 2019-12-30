module Year2019.Day18 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import AStar exposing (Path)
import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import AssocList
import Char
import Dict exposing (Dict)
import Dict.Extra
import List.Extra
import Set exposing (Set)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    State


type alias Input2 =
    State


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


type alias Job =
    { collectedWithoutLast : Set Char
    , lastCollected : Char
    , cost : Int -- of all collected
    , objects : Dict Coord Object
    , position : Coord
    }


type alias State =
    { originalObjects : Dict Coord Object
    , originalPosition : Coord
    , keys : Dict Char Coord
    , keysSet : Set Char
    , jobs : List Job
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
            (\( x, y, char ) state ->
                let
                    newPosition =
                        if char == '@' then
                            ( x, y )

                        else
                            state.originalPosition

                    ( newKeys, newKeysSet ) =
                        if Char.isLower char then
                            ( Dict.insert char ( x, y ) state.keys
                            , Set.insert char state.keysSet
                            )

                        else
                            ( state.keys
                            , state.keysSet
                            )

                    newObjects =
                        Dict.insert
                            ( x, y )
                            (parseObject char)
                            state.originalObjects
                in
                { state
                    | originalObjects = newObjects
                    , originalPosition = newPosition
                    , keys = newKeys
                    , keysSet = newKeysSet
                }
            )
            { originalObjects = Dict.empty
            , originalPosition = ( 0, 0 )
            , keys = Dict.empty
            , keysSet = Set.empty
            , jobs = []
            }
        |> addStartingJobs


reachableKeyPaths : Coord -> Dict Char Coord -> List Char -> Dict Coord Object -> List ( Char, Coord, Path )
reachableKeyPaths coord keys neededKeys objects =
    neededKeys
        |> List.filterMap
            (\key ->
                Dict.get key keys
                    |> Maybe.andThen
                        (\keyCoord ->
                            pathToKey coord keyCoord objects
                                |> Maybe.map (\path -> ( key, keyCoord, path ))
                        )
            )


addStartingJobs : State -> State
addStartingJobs state =
    let
        paths : List ( Char, Coord, Path )
        paths =
            reachableKeyPaths
                state.originalPosition
                state.keys
                (Dict.keys state.keys)
                state.originalObjects

        jobs : List Job
        jobs =
            paths
                |> List.map
                    (\( key, keyCoord, path ) ->
                        { collectedWithoutLast = Set.empty
                        , lastCollected = key
                        , cost = List.length path
                        , objects = removeDoorAndKey key state.originalObjects
                        , position = keyCoord
                        }
                    )
                |> keepOnlyBestJobs
                |> (\j ->
                        let
                            _ =
                                Debug.log "new jobs" (List.length j)
                        in
                        j
                   )
    in
    { state | jobs = jobs }


keepOnlyBestJobs : List Job -> List Job
keepOnlyBestJobs jobs =
    jobs
        |> Dict.Extra.groupBy (\job -> ( List.sort (Set.toList job.collectedWithoutLast), job.lastCollected ))
        |> Dict.toList
        |> List.map
            (\( chars, jobs_ ) ->
                jobs_
                    |> List.Extra.minimumBy .cost
                    |> Advent.unsafeMaybe "keepOnlyBestJobs"
            )


removeDoorAndKey : Char -> Dict Coord Object -> Dict Coord Object
removeDoorAndKey char objects =
    -- TODO not that efficient, could use `doors : Dict Char Coord`
    objects
        |> Dict.map
            (\_ object ->
                if object == Door char || object == Key char then
                    Floor

                else
                    object
            )


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
                Door (Char.toLower char)

            else
                Debug.todo "parseObject wat"


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : State -> Output1
compute1 state =
    if allKeysCollected state.jobs then
        state.jobs
            |> List.map .cost
            |> List.minimum
            |> Advent.unsafeMaybe "compute1"

    else
        let
            newJobs =
                state.jobs
                    |> List.concatMap
                        (\job ->
                            let
                                paths : List ( Char, Coord, Path )
                                paths =
                                    reachableKeyPaths
                                        job.position
                                        state.keys
                                        (Set.diff state.keysSet job.collectedWithoutLast
                                            |> Set.remove job.lastCollected
                                            |> Set.toList
                                        )
                                        job.objects
                            in
                            paths
                                |> List.map
                                    (\( key, keyCoord, path ) ->
                                        { collectedWithoutLast = Set.insert job.lastCollected job.collectedWithoutLast
                                        , lastCollected = key
                                        , cost = job.cost + List.length path
                                        , objects = removeDoorAndKey key job.objects
                                        , position = keyCoord
                                        }
                                    )
                        )
                    |> keepOnlyBestJobs
                    |> (\j ->
                            let
                                _ =
                                    Debug.log "new jobs" (List.length j)
                            in
                            j
                       )
        in
        compute1 { state | jobs = newJobs }


allKeysCollected : List Job -> Bool
allKeysCollected jobs =
    jobs
        |> List.head
        |> Advent.unsafeMaybe "allKeysCollected"
        |> .objects
        |> Dict.values
        |> List.all (\obj -> obj == Wall || obj == Floor)


pathToKey : Coord -> Coord -> Dict Coord Object -> Maybe Path
pathToKey currentCoord keyCoord objects =
    AStar.findPath
        AStar.straightLineCost
        (movesFrom objects)
        currentCoord
        keyCoord


print : String -> Job -> ()
print log job =
    let
        _ =
            job.objects
                |> Dict.toList
                |> Dict.Extra.groupBy (Tuple.first >> Tuple.second)
                |> Dict.toList
                |> List.sortBy (Tuple.first >> negate)
                |> List.map (Tuple.mapSecond (List.sortBy (Tuple.first >> Tuple.first)))
                |> List.map
                    (Tuple.second
                        >> List.map
                            (\( coord, obj ) ->
                                case obj of
                                    Floor ->
                                        if job.position == coord then
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
