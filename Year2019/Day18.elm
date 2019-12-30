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
import Graph exposing (Edge, Graph)
import List.Extra
import Set exposing (Set)



-- 1. TYPES (what is the best representation of the problem?)


type alias Coord =
    ( Int, Int )


type Object
    = Wall
    | Door Char
    | Key Char
    | Floor


type alias RawData1 =
    { objects : Dict Coord Object
    , position : Coord
    , keys : Dict Char Coord
    }


type alias RawData2 =
    { objects : Dict Coord Object
    , position1 : Coord
    , position2 : Coord
    , position3 : Coord
    , position4 : Coord
    , keys : Dict Char Coord
    }


type alias Maze =
    Graph Char ( Int, Set Char )


type alias State1 =
    { maze : Maze
    , jobs : List Job1
    }


type alias State2 =
    { maze : Maze
    , jobs : List Job2
    }


type alias Job1 =
    { collected : Set Char
    , current : Char
    , cost : Int
    }


type alias Job2 =
    { collected : Set Char
    , current1 : Char
    , current2 : Char
    , current3 : Char
    , current4 : Char
    , cost : Int
    }


type alias Input1 =
    State1


type alias Input2 =
    State2


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


log : String -> a -> a
log label x =
    let
        _ =
            Debug.log label ()
    in
    x


parse1 : String -> Input1
parse1 string =
    string
        |> toRawData1
        |> log "got raw data"
        |> toMaze1
        |> log "got maze"
        |> toState1
        |> log "got state"


toMaze1 : RawData1 -> Maze
toMaze1 rawData =
    let
        keys =
            Dict.toList rawData.keys
                |> log "keys"

        playerEdges =
            keys
                |> List.filterMap
                    (\( key, keyCoord ) ->
                        let
                            _ =
                                Debug.log "1 finding for" ( '@', key )
                        in
                        AStar.findPath
                            AStar.straightLineCost
                            (movesFrom rawData.objects)
                            rawData.position
                            keyCoord
                            |> Maybe.map
                                (\path_ ->
                                    let
                                        cost =
                                            List.length path_

                                        deps =
                                            findDeps rawData.objects path_
                                    in
                                    { from = '@'
                                    , to = key
                                    , data = ( cost, deps )
                                    }
                                )
                    )
                |> log "player edges"

        keyEdges =
            keys
                |> List.Extra.uniquePairs
                |> List.concatMap
                    (\( ( a, aCoord ), ( b, bCoord ) ) ->
                        let
                            _ =
                                Debug.log "1 finding for" ( a, b )
                        in
                        let
                            path =
                                AStar.findPath
                                    AStar.straightLineCost
                                    (movesFrom rawData.objects)
                                    aCoord
                                    bCoord
                                    |> Advent.unsafeMaybe "rawData to maze - find key path"

                            cost =
                                List.length path

                            deps =
                                findDeps rawData.objects path
                        in
                        [ { from = a, to = b, data = ( cost, deps ) }
                        , { from = b, to = a, data = ( cost, deps ) }
                        ]
                    )
                |> log "key edges"
    in
    Graph.fromVerticesAndEdges [] (playerEdges ++ keyEdges)
        |> log "graph"


toMaze2 : RawData2 -> Maze
toMaze2 rawData =
    let
        keys =
            Dict.toList rawData.keys
                |> log "keys"

        playerEdges =
            [ ( '1', rawData.position1 )
            , ( '2', rawData.position2 )
            , ( '3', rawData.position3 )
            , ( '4', rawData.position4 )
            ]
                |> List.concatMap
                    (\( player, playerCoord ) ->
                        keys
                            |> List.filterMap
                                (\( key, keyCoord ) ->
                                    let
                                        _ =
                                            Debug.log "2 finding for" ( player, key )
                                    in
                                    AStar.findPath
                                        AStar.straightLineCost
                                        (movesFrom rawData.objects)
                                        playerCoord
                                        keyCoord
                                        |> Maybe.map
                                            (\path ->
                                                let
                                                    cost =
                                                        List.length path

                                                    deps =
                                                        findDeps rawData.objects path
                                                in
                                                { from = player
                                                , to = key
                                                , data = ( cost, deps )
                                                }
                                            )
                                )
                    )
                |> log "player edges"

        keyEdges =
            keys
                |> List.Extra.uniquePairs
                |> List.filterMap
                    (\( ( a, aCoord ), ( b, bCoord ) ) ->
                        let
                            _ =
                                Debug.log "2 finding for" ( a, b )
                        in
                        AStar.findPath
                            AStar.straightLineCost
                            (movesFrom rawData.objects)
                            aCoord
                            bCoord
                            |> Maybe.map
                                (\path ->
                                    let
                                        cost =
                                            List.length path

                                        deps =
                                            findDeps rawData.objects path
                                    in
                                    [ { from = a, to = b, data = ( cost, deps ) }
                                    , { from = b, to = a, data = ( cost, deps ) }
                                    ]
                                )
                    )
                |> List.concat
                |> log "key edges"
    in
    Graph.fromVerticesAndEdges [] (playerEdges ++ keyEdges)
        |> log "graph"


toState1 : Maze -> State1
toState1 maze =
    { maze = maze
    , jobs =
        [ { collected = Set.singleton '@'
          , current = '@'
          , cost = 0
          }
        ]
    }


toState2 : Maze -> State2
toState2 maze =
    { maze = maze
    , jobs =
        [ { collected = Set.fromList [ '1', '2', '3', '4' ]
          , current1 = '1'
          , current2 = '2'
          , current3 = '3'
          , current4 = '4'
          , cost = 0
          }
        ]
    }


findDeps : Dict Coord Object -> Path -> Set Char
findDeps objects path =
    path
        |> List.filterMap
            (\coord ->
                Dict.get coord objects
                    |> Maybe.andThen keepDoors
            )
        |> Set.fromList


keepDoors : Object -> Maybe Char
keepDoors obj =
    case obj of
        Door c ->
            Just c

        _ ->
            Nothing


movesFrom : Dict Coord Object -> Coord -> Set Coord
movesFrom objects ( x, y ) =
    [ ( x - 1, y )
    , ( x + 1, y )
    , ( x, y - 1 )
    , ( x, y + 1 )
    ]
        |> Set.fromList
        |> Set.filter (\coord -> Dict.get coord objects /= Just Wall)


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
            if Char.isDigit char then
                Floor

            else if Char.isLower char then
                Key char

            else if Char.isUpper char then
                Door (Char.toLower char)

            else
                Debug.todo "parseObject wat"


parse2 : String -> Input2
parse2 string =
    string
        |> changeToFourVaults
        |> log "changed to four vaults"
        |> toRawData2
        |> log "got raw data"
        |> toMaze2
        |> log "got maze"
        |> toState2
        |> log "got state"


changeToFourVaults : String -> String
changeToFourVaults string =
    if String.all ((==) '#') (String.left 81 string) then
        patchedInput

    else
        string


toRawData1 : String -> RawData1
toRawData1 string =
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
            (\( x, y, char ) rawData ->
                let
                    newPosition =
                        if char == '@' then
                            ( x, y )

                        else
                            rawData.position

                    newKeys =
                        if Char.isLower char then
                            Dict.insert char ( x, y ) rawData.keys

                        else
                            rawData.keys

                    newObjects =
                        Dict.insert
                            ( x, y )
                            (parseObject char)
                            rawData.objects
                in
                { rawData
                    | objects = newObjects
                    , position = newPosition
                    , keys = newKeys
                }
            )
            { objects = Dict.empty
            , position = ( 0, 0 )
            , keys = Dict.empty
            }


toRawData2 : String -> RawData2
toRawData2 string =
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
            (\( x, y, char ) rawData ->
                let
                    newPosition1 =
                        if char == '1' then
                            ( x, y )

                        else
                            rawData.position1

                    newPosition2 =
                        if char == '2' then
                            ( x, y )

                        else
                            rawData.position2

                    newPosition3 =
                        if char == '3' then
                            ( x, y )

                        else
                            rawData.position3

                    newPosition4 =
                        if char == '4' then
                            ( x, y )

                        else
                            rawData.position4

                    newKeys =
                        if Char.isLower char then
                            Dict.insert char ( x, y ) rawData.keys

                        else
                            rawData.keys

                    newObjects =
                        Dict.insert
                            ( x, y )
                            (parseObject char)
                            rawData.objects
                in
                { rawData
                    | objects = newObjects
                    , position1 = newPosition1
                    , position2 = newPosition2
                    , position3 = newPosition3
                    , position4 = newPosition4
                    , keys = newKeys
                }
            )
            { objects = Dict.empty
            , position1 = ( 0, 0 )
            , position2 = ( 0, 0 )
            , position3 = ( 0, 0 )
            , position4 = ( 0, 0 )
            , keys = Dict.empty
            }



-- 3. COMPUTE (actually solve the problem)


compute1 : State1 -> Output1
compute1 state =
    let
        _ =
            Debug.log "1 jobs"
                (List.length state.jobs
                 --, List.map jobToString state.jobs
                )
    in
    if canStop1 state then
        state.jobs
            |> List.map .cost
            |> List.minimum
            |> Advent.unsafeMaybe "compute1 minimum"

    else
        let
            newJobs =
                state.jobs
                    |> List.concatMap
                        (\job ->
                            state.maze
                                |> Graph.outgoingEdgesWithData job.current
                                |> List.filterMap
                                    (\( char, ( cost, deps ) ) ->
                                        if Set.member char job.collected then
                                            Nothing

                                        else if Set.isEmpty (Set.diff deps job.collected) then
                                            Just
                                                { collected = Set.insert char job.collected
                                                , current = char
                                                , cost = job.cost + cost
                                                }

                                        else
                                            Nothing
                                    )
                        )
                    |> keepOnlyBestJobs1
        in
        compute1 { state | jobs = newJobs }


job1ToString : Job1 -> String
job1ToString job =
    String.fromList (List.sort (Set.toList job.collected))
        ++ ":"
        ++ String.fromChar job.current


keepOnlyBestJobs1 : List Job1 -> List Job1
keepOnlyBestJobs1 jobs =
    jobs
        |> Dict.Extra.groupBy (\job -> ( List.sort (Set.toList job.collected), job.current ))
        |> Dict.toList
        |> List.map
            (\( chars, jobs_ ) ->
                jobs_
                    |> List.Extra.minimumBy .cost
                    |> Advent.unsafeMaybe "keepOnlyBestJobs1"
            )


canStop1 : State1 -> Bool
canStop1 state =
    let
        firstJob =
            state.jobs
                |> List.head
                |> Advent.unsafeMaybe "canStop1"
    in
    Set.isEmpty
        (Set.diff
            (Set.fromList (Graph.vertices state.maze))
            firstJob.collected
        )


compute2 : State2 -> Output2
compute2 state =
    let
        _ =
            Debug.log "2 jobs"
                (List.length state.jobs
                 --, List.map jobToString state.jobs
                )
    in
    if canStop2 state then
        state.jobs
            |> List.map .cost
            |> List.minimum
            |> Advent.unsafeMaybe "compute2 minimum"

    else
        let
            newJobs =
                state.jobs
                    |> List.concatMap
                        (\job ->
                            let
                                setter1 x job_ =
                                    { job_ | current1 = x }

                                setter2 x job_ =
                                    { job_ | current2 = x }

                                setter3 x job_ =
                                    { job_ | current3 = x }

                                setter4 x job_ =
                                    { job_ | current4 = x }

                                jobsFor : (Char -> Job2 -> Job2) -> Char -> List Job2
                                jobsFor setter current =
                                    state.maze
                                        |> Graph.outgoingEdgesWithData current
                                        |> List.filterMap
                                            (\( char, ( cost, deps ) ) ->
                                                if Set.member char job.collected then
                                                    Nothing

                                                else if Set.isEmpty (Set.diff deps job.collected) then
                                                    { job
                                                        | collected = Set.insert char job.collected
                                                        , cost = job.cost + cost
                                                    }
                                                        |> setter char
                                                        |> Just

                                                else
                                                    Nothing
                                            )
                            in
                            [ jobsFor setter1 job.current1
                            , jobsFor setter2 job.current2
                            , jobsFor setter3 job.current3
                            , jobsFor setter4 job.current4
                            ]
                                |> List.concat
                        )
                    |> keepOnlyBestJobs2
        in
        compute2 { state | jobs = newJobs }


job2ToString : Job2 -> String
job2ToString job =
    String.fromList (List.sort (Set.toList job.collected))
        ++ ":"
        ++ String.fromChar job.current1
        ++ String.fromChar job.current2
        ++ String.fromChar job.current3
        ++ String.fromChar job.current4


keepOnlyBestJobs2 : List Job2 -> List Job2
keepOnlyBestJobs2 jobs =
    jobs
        |> Dict.Extra.groupBy
            (\job ->
                ( List.sort (Set.toList job.collected)
                , [ job.current1
                  , job.current2
                  , job.current3
                  , job.current4
                  ]
                )
            )
        |> Dict.toList
        |> List.map
            (\( chars, jobs_ ) ->
                jobs_
                    |> List.Extra.minimumBy .cost
                    |> Advent.unsafeMaybe "keepOnlyBestJobs2"
            )


canStop2 : State2 -> Bool
canStop2 state =
    let
        firstJob =
            state.jobs
                |> List.head
                |> Advent.unsafeMaybe "canStop2"
    in
    Set.isEmpty
        (Set.diff
            (Set.fromList (Graph.vertices state.maze))
            firstJob.collected
        )



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
    [ Test "ex 2 1"
        """#######
#a.#Cd#
##1#3##
#######
##2#4##
#cB#Ab#
#######"""
        Nothing
        8
    , Test "ex 2 2"
        """###############
#d.ABC.#.....a#
######1#3######
###############
######2#4######
#b.....#.....c#
###############"""
        Nothing
        24
    , Test "ex 2 3"
        """#############
#DcBa.#.GhKl#
#.###1#3#I###
#e#d#####j#k#
###C#2#4###J#
#fEbA.#.FgHi#
#############"""
        Nothing
        32
    , Test "ex 2 4"
        """#############
#g#f.D#..h#l#
#F###e#E###.#
#dCba1#3BcIJ#
#############
#nK.L2#4G...#
#M###N#H###.#
#o#m..#i#jk.#
#############"""
        Nothing
        72
    ]



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


patchedInput : String
patchedInput =
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
#.............#.........#.........#....1#3................#.............#.......#
#################################################################################
#.........#...#.........#.....#...#....2#4....#...........#..q....#.....#.......#
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
