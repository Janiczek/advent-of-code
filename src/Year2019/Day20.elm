module Year2019.Day20 exposing (..)

-- day 18 is similar to this
-- A bit of input preprocessing was needed to simplify the comptation (no two-letter portals)

import AStar exposing (Path)
import AStar.Generalised
import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Char
import Dict exposing (Dict)
import Dict.Extra
import Graph exposing (Edge, Graph)
import List.Extra
import Maybe.Extra
import Set exposing (Set)



-- 1. TYPES (what is the best representation of the problem?)


type alias Coord =
    ( Int, Int )


type alias Maze1 =
    Graph String Int


type alias Maze2 =
    -- (cost, levels change)
    -- Graph String ( Int, Int )
    Maze1


{-| these are OUTSIDE the maze! Act accordingly
-}
type Portal
    = Start Coord -- AA, always outer
    | End Coord -- ZZ, always outer
    | Inner Coord
    | Outer Coord


type alias State1 =
    { maze : Maze1
    , jobs : List Job1
    , best : Dict String Int
    }


type alias State2 =
    { maze : Maze1
    , jobs : List Job2
    , best : Dict ( String, Int ) Int
    }


type alias Job1 =
    { cost : Int
    , current : String
    }


type alias Job2 =
    { cost : Int
    , current : String
    , currentLevel : Int
    }


type alias Input1 =
    State1


type alias Input2 =
    State2


type alias Output1 =
    Int


type alias Output2 =
    Int


log : String -> a -> a
log label x =
    let
        _ =
            Debug.log label ()
    in
    x



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    -- toMaze1 took too long so I kept the result after the first processing
    {-
       string
           |> toRawData
           |> toMaze1
           |> toState1
    -}
    toState1 parsedMaze


parse2 : String -> Input2
parse2 string =
    -- see comment in parse1
    toState2 parsedMaze2


type alias RawData =
    { floor : Set Coord
    , portals : List ( Char, Portal )
    }


toRawData : String -> RawData
toRawData string =
    let
        width : Int
        width =
            string
                |> String.lines
                |> List.drop 1
                |> List.head
                |> Advent.unsafeMaybe "parse1 width"
                |> String.length

        height : Int
        height =
            string
                |> String.lines
                |> List.length

        isOuterPortal : Coord -> Bool
        isOuterPortal ( x, y ) =
            x == 0 || y == 0 || x == width - 1 || y == height - 1

        rawList : List ( Coord, Char )
        rawList =
            string
                |> String.lines
                |> List.indexedMap
                    (\y line ->
                        line
                            |> String.toList
                            |> List.indexedMap (\x char -> ( ( x, y ), char ))
                    )
                |> List.concat

        floor : Set Coord
        floor =
            rawList
                |> List.filterMap
                    (\( coord, char ) ->
                        if char == '.' then
                            Just coord

                        else
                            Nothing
                    )
                |> Set.fromList

        dot_ : Coord -> Coord
        dot_ coord =
            dotNextTo coord floor

        portals : List ( Char, Portal )
        portals =
            rawList
                |> List.filterMap
                    (\( coord, char ) ->
                        if Char.isAlpha char then
                            let
                                coord_ =
                                    dot_ coord
                            in
                            if char == 'A' then
                                Just ( char, Start coord_ )

                            else if char == 'Z' then
                                Just ( char, End coord_ )

                            else
                                Just
                                    ( char
                                    , if isOuterPortal coord then
                                        Outer coord_

                                      else
                                        Inner coord_
                                    )

                        else
                            Nothing
                    )
    in
    { floor = floor
    , portals = portals
    }


toString : Char -> Portal -> String
toString char portal =
    case portal of
        Inner _ ->
            String.fromChar char ++ "I"

        Outer _ ->
            String.fromChar char ++ "O"

        _ ->
            String.fromChar char


toMaze1 : Set Coord -> List ( Char, Portal ) -> Maze1
toMaze1 floor portals =
    let
        pathLength : Coord -> Coord -> Maybe Int
        pathLength from to =
            AStar.findPath
                (\_ _ -> 1)
                (movesFrom floor)
                from
                to
                |> Maybe.map List.length
                |> Debug.log ("path length " ++ Debug.toString ( from, to ))

        interPortal : List (Edge String Int)
        interPortal =
            portals
                |> List.filter (\( char, _ ) -> char /= 'A' && char /= 'Z')
                |> List.concatMap
                    (\( char, _ ) ->
                        let
                            inner =
                                String.fromChar char ++ "I"

                            outer =
                                String.fromChar char ++ "O"
                        in
                        [ { from = inner, to = outer, data = 1 }
                        , { from = outer, to = inner, data = 1 }
                        ]
                    )

        twoPortals : List (Edge String Int)
        twoPortals =
            portals
                -- TODO this could get a lot faster if we just somehow flooded it
                -- (for portal X, find `List (Portal, DistanceToIt)` of reachable other portals)
                |> List.Extra.uniquePairs
                |> log "unique pairs"
                |> List.concatMap
                    (\( ( char1, portal1 ), ( char2, portal2 ) ) ->
                        let
                            string1 =
                                toString char1 portal1

                            string2 =
                                toString char2 portal2

                            toEdges length =
                                if char1 == 'Z' then
                                    [ { from = string2
                                      , to = string1
                                      , data = length
                                      }
                                    ]

                                else if char2 == 'Z' then
                                    [ { from = string1
                                      , to = string2
                                      , data = length
                                      }
                                    ]

                                else
                                    [ { from = string1
                                      , to = string2
                                      , data = length
                                      }
                                    , { from = string2
                                      , to = string1
                                      , data = length
                                      }
                                    ]
                        in
                        (case ( portal1, portal2 ) of
                            ( Start _, Start _ ) ->
                                Debug.todo "wat 1"

                            ( Start startCoord, End endCoord ) ->
                                [ pathLength startCoord endCoord ]

                            ( Start startCoord, Inner innerCoord ) ->
                                [ pathLength startCoord innerCoord ]

                            ( Start startCoord, Outer outerCoord ) ->
                                [ pathLength startCoord outerCoord ]

                            ( End endCoord, Start startCoord ) ->
                                [ pathLength endCoord startCoord ]

                            ( End _, End _ ) ->
                                Debug.todo "wat 2"

                            ( End endCoord, Inner innerCoord ) ->
                                [ pathLength endCoord innerCoord ]

                            ( End endCoord, Outer outerCoord ) ->
                                [ pathLength endCoord outerCoord ]

                            ( Inner innerCoord, Start startCoord ) ->
                                [ pathLength innerCoord startCoord ]

                            ( Inner innerCoord, End endCoord ) ->
                                [ pathLength innerCoord endCoord ]

                            ( Inner innerCoord, Inner innerCoord2 ) ->
                                [ pathLength innerCoord innerCoord2 ]

                            ( Inner innerCoord, Outer outerCoord ) ->
                                [ pathLength innerCoord outerCoord ]

                            ( Outer outerCoord, Start startCoord ) ->
                                [ pathLength outerCoord startCoord ]

                            ( Outer outerCoord, End endCoord ) ->
                                [ pathLength outerCoord endCoord ]

                            ( Outer outerCoord, Inner innerCoord ) ->
                                [ pathLength outerCoord innerCoord ]

                            ( Outer outerCoord, Outer outerCoord2 ) ->
                                [ pathLength outerCoord outerCoord2 ]
                        )
                            |> List.filterMap identity
                            |> List.concatMap toEdges
                    )
                |> log "two portals"
    in
    Graph.fromVerticesAndEdges [] (interPortal ++ twoPortals)


neighbours : Coord -> List Coord
neighbours ( x, y ) =
    [ ( x - 1, y )
    , ( x + 1, y )
    , ( x, y - 1 )
    , ( x, y + 1 )
    ]


dotNextTo : Coord -> Set Coord -> Coord
dotNextTo coord floor =
    neighbours coord
        |> List.Extra.find (\coord_ -> Set.member coord_ floor)
        |> Advent.unsafeMaybe ("dot next to " ++ Debug.toString coord)


toState2 : Maze2 -> State2
toState2 maze =
    { maze = maze
    , jobs =
        [ { cost = 0
          , current = "A"
          , currentLevel = 0
          }
        ]
    , best = Dict.empty
    }



-- 3. COMPUTE (actually solve the problem)


toState1 : Maze1 -> State1
toState1 maze =
    { maze = maze
    , jobs = [ { cost = 0, current = "A" } ]
    , best = Dict.empty
    }


compute1 : Input1 -> Output1
compute1 state =
    if List.isEmpty state.jobs then
        state.best
            |> Dict.get "Z"
            |> Advent.unsafeMaybe "compute1"

    else
        let
            newJobs =
                state.jobs
                    |> List.concatMap
                        (\job ->
                            state.maze
                                |> Graph.outgoingEdgesWithData job.current
                                |> List.filterMap
                                    (\( destination, cost ) ->
                                        let
                                            newCost =
                                                job.cost + cost
                                        in
                                        if newCost < get_ destination state.best then
                                            Just
                                                { current = destination
                                                , cost = newCost
                                                }

                                        else
                                            Nothing
                                    )
                        )

            newBest =
                List.foldl
                    (\job accBest ->
                        Dict.insert job.current job.cost accBest
                    )
                    state.best
                    newJobs
        in
        compute1
            { state
                | best = newBest
                , jobs = newJobs
            }


get_ : comparable -> Dict comparable Int -> Int
get_ x best =
    Dict.get x best
        |> Maybe.withDefault 9999999


movesFrom : Set Coord -> Coord -> Set Coord
movesFrom floor coord =
    neighbours coord
        |> Set.fromList
        |> Set.intersect floor


isInner : String -> Bool
isInner vertex =
    String.endsWith "I" vertex


isOuter : String -> Bool
isOuter vertex =
    String.endsWith "O" vertex


jobToString : Job2 -> String
jobToString job =
    -- ++ "(" ++ String.fromInt job.cost ++ ")"
    job.current ++ String.fromInt job.currentLevel


jobsToString : List Job2 -> String
jobsToString jobs =
    jobs
        |> List.map jobToString
        |> String.join ", "


path : List String
path =
    -- Done by hand, with pen and paper. Don't ask.
    "A,OI,OO,PI,PO,XI,XO,JI,JO,RI,RO,LI,LO,VI,VO,GI,GO,bI,bO,MI,MO,TI,TO,FI,FO,cI,cO,SI,SO,WI,WO,UI,UO,II,IO,EI,EO,dI,dO,NI,NO,CI,CO,aI,aO,XO,XI,PO,PI,OO,OI,HO,HI,DO,DI,YO,YI,BO,BI,KO,KI,JI,JO,RI,RO,LI,LO,VI,VO,GI,GO,bI,bO,MI,MO,TI,TO,FI,FO,cI,cO,SI,SO,WI,WO,UI,UO,II,IO,EI,EO,dI,dO,NI,NO,CI,CO,aI,aO,XO,XI,PO,PI,OO,OI,HO,HI,DO,DI,YO,YI,BO,BI,KO,KI,JI,JO,RI,RO,LI,LO,VI,VO,GI,GO,bI,bO,MI,MO,TI,TO,FI,FO,cI,cO,SI,SO,WI,WO,UI,UO,II,IO,EI,EO,dI,dO,NI,NO,CI,CO,aI,aO,XO,XI,PO,PI,OO,OI,HO,HI,DO,DI,YO,YI,BO,BI,KO,KI,XO,XI,PO,PI,OO,OI,HO,HI,DO,DI,YO,YI,BO,BI,KO,KI,XO,XI,PO,PI,OO,OI,HO,HI,DO,DI,YO,YI,BO,BI,KO,KI,XO,XI,PO,PI,OO,OI,HO,HI,DO,DI,YO,YI,BO,BI,KO,KI,aO,aI,CO,CI,NO,NI,dO,dI,EO,EI,IO,II,UO,UI,WO,WI,SO,SI,cO,cI,FO,FI,TO,TI,Z"
        |> String.split ","


pathCost : Maze2 -> List String -> Int
pathCost maze vertices =
    vertices
        |> List.Extra.groupsOfWithStep 2 1
        |> List.map
            (\group ->
                case group of
                    [ a, b ] ->
                        Graph.getEdge a b maze

                    _ ->
                        Debug.todo "pathCost wat"
            )
        |> Maybe.Extra.combine
        |> Advent.unsafeMaybe "pathCost"
        |> List.sum


compute2 : Input2 -> Output2
compute2 state =
    pathCost state.maze path



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example"
                (Advent.removeNewlinesAtEnds """
                A
         #######.#########
         #######.........#
         #######.#######.#
         #######.#######.#
         #######.#######.#
         #####  D    ###.#
        D...##       ###.#
         ##.##       ###.#
         ##...B      ###.#
         #####    C  ###.#
         #########.#####.#
        B..#######...###.#
         #.#########.###.#
        C..#########.....#
         ###########.#####
                    Z
        """)
                Nothing
                23
     -}
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


parsedEdges2 : List (Edge String ( Int, Int ))
parsedEdges2 =
    [ { from = "HO", to = "KI", data = ( 235, -5 ) }
    , { from = "KI", to = "HO", data = ( 235, 5 ) }
    , { from = "JI", to = "MO", data = ( 365, 7 ) }
    , { from = "MO", to = "JI", data = ( 365, -7 ) }
    , { from = "aO", to = "TI", data = ( 746, -12 ) }
    , { from = "TI", to = "aO", data = ( 746, 12 ) }
    , { from = "XO", to = "OI", data = ( 111, -3 ) }
    , { from = "OI", to = "XO", data = ( 111, 3 ) }
    , { from = "A", to = "HO", data = ( 12, 0 ) }
    , { from = "A", to = "OI", data = ( 66, 0 ) }
    , { from = "MO", to = "TI", data = ( 62, 0 ) }
    , { from = "MO", to = "Z", data = ( 4, 0 ) }
    , { from = "TI", to = "MO", data = ( 62, 0 ) }
    , { from = "TI", to = "Z", data = ( 64, 0 ) }
    , { from = "XO", to = "aO", data = ( 4, 0 ) }
    , { from = "XO", to = "JI", data = ( 44, 0 ) }
    , { from = "XO", to = "KI", data = ( 48, 0 ) }
    , { from = "KI", to = "XO", data = ( 48, 0 ) }
    , { from = "KI", to = "aO", data = ( 50, 0 ) }
    , { from = "KI", to = "JI", data = ( 6, 0 ) }
    , { from = "JI", to = "XO", data = ( 44, 0 ) }
    , { from = "JI", to = "aO", data = ( 46, 0 ) }
    , { from = "JI", to = "KI", data = ( 6, 0 ) }
    , { from = "OI", to = "HO", data = ( 56, 0 ) }
    , { from = "OI", to = "A", data = ( 66, 0 ) }
    , { from = "HO", to = "OI", data = ( 56, 0 ) }
    , { from = "HO", to = "A", data = ( 12, 0 ) }
    , { from = "aO", to = "XO", data = ( 4, 0 ) }
    , { from = "aO", to = "JI", data = ( 46, 0 ) }
    , { from = "aO", to = "KI", data = ( 50, 0 ) }
    ]


parsedEdges : List (Edge String Int)
parsedEdges =
    [ { data = 12, from = "A", to = "HO" }
    , { data = 66, from = "A", to = "OI" }
    , { data = 78, from = "WO", to = "UI" }
    , { data = 1, from = "WO", to = "WI" }
    , { data = 88, from = "WI", to = "SO" }
    , { data = 1, from = "WI", to = "WO" }
    , { data = 56, from = "UO", to = "II" }
    , { data = 1, from = "UO", to = "UI" }
    , { data = 78, from = "UI", to = "WO" }
    , { data = 1, from = "UI", to = "UO" }
    , { data = 62, from = "MO", to = "TI" }
    , { data = 4, from = "MO", to = "Z" }
    , { data = 1, from = "MO", to = "MI" }
    , { data = 54, from = "MI", to = "bO" }
    , { data = 1, from = "MI", to = "MO" }
    , { data = 86, from = "TO", to = "FI" }
    , { data = 1, from = "TO", to = "TI" }
    , { data = 62, from = "TI", to = "MO" }
    , { data = 64, from = "TI", to = "Z" }
    , { data = 1, from = "TI", to = "TO" }
    , { data = 40, from = "NO", to = "CI" }
    , { data = 1, from = "NO", to = "NI" }
    , { data = 46, from = "NI", to = "dO" }
    , { data = 1, from = "NI", to = "NO" }
    , { data = 52, from = "VO", to = "GI" }
    , { data = 1, from = "VO", to = "VI" }
    , { data = 74, from = "VI", to = "LO" }
    , { data = 1, from = "VI", to = "VO" }
    , { data = 4, from = "XO", to = "aO" }
    , { data = 44, from = "XO", to = "JI" }
    , { data = 48, from = "XO", to = "KI" }
    , { data = 1, from = "XO", to = "XI" }
    , { data = 48, from = "XI", to = "PO" }
    , { data = 1, from = "XI", to = "XO" }
    , { data = 70, from = "KO", to = "BI" }
    , { data = 1, from = "KO", to = "KI" }
    , { data = 48, from = "KI", to = "XO" }
    , { data = 50, from = "KI", to = "aO" }
    , { data = 6, from = "KI", to = "JI" }
    , { data = 1, from = "KI", to = "KO" }
    , { data = 88, from = "SO", to = "WI" }
    , { data = 1, from = "SO", to = "SI" }
    , { data = 52, from = "SI", to = "cO" }
    , { data = 1, from = "SI", to = "SO" }
    , { data = 50, from = "JO", to = "RI" }
    , { data = 1, from = "JO", to = "JI" }
    , { data = 44, from = "JI", to = "XO" }
    , { data = 46, from = "JI", to = "aO" }
    , { data = 6, from = "JI", to = "KI" }
    , { data = 1, from = "JI", to = "JO" }
    , { data = 62, from = "RO", to = "LI" }
    , { data = 1, from = "RO", to = "RI" }
    , { data = 50, from = "RI", to = "JO" }
    , { data = 1, from = "RI", to = "RO" }
    , { data = 68, from = "IO", to = "EI" }
    , { data = 1, from = "IO", to = "II" }
    , { data = 56, from = "II", to = "UO" }
    , { data = 1, from = "II", to = "IO" }
    , { data = 74, from = "LO", to = "VI" }
    , { data = 1, from = "LO", to = "LI" }
    , { data = 62, from = "LI", to = "RO" }
    , { data = 1, from = "LI", to = "LO" }
    , { data = 48, from = "PO", to = "XI" }
    , { data = 1, from = "PO", to = "PI" }
    , { data = 60, from = "PI", to = "OO" }
    , { data = 1, from = "PI", to = "PO" }
    , { data = 60, from = "OO", to = "PI" }
    , { data = 1, from = "OO", to = "OI" }
    , { data = 56, from = "OI", to = "HO" }
    , { data = 66, from = "OI", to = "A" }
    , { data = 1, from = "OI", to = "OO" }
    , { data = 54, from = "bO", to = "MI" }
    , { data = 1, from = "bO", to = "bI" }
    , { data = 66, from = "bI", to = "GO" }
    , { data = 1, from = "bI", to = "bO" }
    , { data = 52, from = "cO", to = "SI" }
    , { data = 1, from = "cO", to = "cI" }
    , { data = 66, from = "cI", to = "FO" }
    , { data = 1, from = "cI", to = "cO" }
    , { data = 46, from = "dO", to = "NI" }
    , { data = 1, from = "dO", to = "dI" }
    , { data = 74, from = "dI", to = "EO" }
    , { data = 1, from = "dI", to = "dO" }
    , { data = 56, from = "HO", to = "OI" }
    , { data = 12, from = "HO", to = "A" }
    , { data = 1, from = "HO", to = "HI" }
    , { data = 48, from = "HI", to = "DO" }
    , { data = 1, from = "HI", to = "HO" }
    , { data = 4, from = "aO", to = "XO" }
    , { data = 46, from = "aO", to = "JI" }
    , { data = 50, from = "aO", to = "KI" }
    , { data = 1, from = "aO", to = "aI" }
    , { data = 80, from = "aI", to = "CO" }
    , { data = 1, from = "aI", to = "aO" }
    , { data = 48, from = "YO", to = "DI" }
    , { data = 1, from = "YO", to = "YI" }
    , { data = 64, from = "YI", to = "BO" }
    , { data = 1, from = "YI", to = "YO" }
    , { data = 66, from = "GO", to = "bI" }
    , { data = 1, from = "GO", to = "GI" }
    , { data = 52, from = "GI", to = "VO" }
    , { data = 1, from = "GI", to = "GO" }
    , { data = 66, from = "FO", to = "cI" }
    , { data = 1, from = "FO", to = "FI" }
    , { data = 86, from = "FI", to = "TO" }
    , { data = 1, from = "FI", to = "FO" }
    , { data = 74, from = "EO", to = "dI" }
    , { data = 1, from = "EO", to = "EI" }
    , { data = 68, from = "EI", to = "IO" }
    , { data = 1, from = "EI", to = "EO" }
    , { data = 48, from = "DO", to = "HI" }
    , { data = 1, from = "DO", to = "DI" }
    , { data = 48, from = "DI", to = "YO" }
    , { data = 1, from = "DI", to = "DO" }
    , { data = 80, from = "CO", to = "aI" }
    , { data = 1, from = "CO", to = "CI" }
    , { data = 40, from = "CI", to = "NO" }
    , { data = 1, from = "CI", to = "CO" }
    , { data = 64, from = "BO", to = "YI" }
    , { data = 1, from = "BO", to = "BI" }
    , { data = 70, from = "BI", to = "KO" }
    , { data = 1, from = "BI", to = "BO" }
    ]


parsedMaze : Maze1
parsedMaze =
    parsedEdges
        |> Graph.fromVerticesAndEdges []


parsedMaze2 : Maze1
parsedMaze2 =
    parsedEdges
        |> Graph.fromVerticesAndEdges []


input_ : String
input_ =
    """
                                          B     C       D         E     F         G                                          
 #########################################.#####.#######.#########.#####.#########.######################################### 
 #...#...#.#.........#.#.#...#.#.............#.......#.....#...#.#.....#.....#.#.................#.#.#...#...#.#...#.......# 
 #.###.###.###.#####.#.#.###.#.#.#.###.###.#.#######.#.###.#.###.###.###.###.#.#####.#.#####.#.###.#.#.###.###.#.###.###.### 
 #.............#.................#.#.....#.#.#.......#...#.#.......#.#.#.#...#.......#.....#.#.#.#.............#.#...#...#.# 
 #####.###.###.###.#.###.#.#.#.#.#######.#.#######.#####.#####.###.#.#.###.###.###.#.#.#########.#.#.#.#####.###.#.#######.# 
 #.#...#...#...#...#.#...#.#.#.#...#.....#.#.....#.#.#.......#...#...#...#.#.#...#.#.#.............#.#.....#...#.#.....#...# 
 #.#######.#####.#.###########.###.###.#######.#.#.#.###.#####.#.#####.#.#.#.###.###.#.#.#.###.#.###.###.#######.#.#######.# 
 #.#.#.#...#...#.#.#...........#...#.....#.#...#.....#.....#...#.....#.#.....#.....#.#.#.#...#.#...#.#.....#.........#.#...# 
 #.#.#.#######.#.#####.###.#.#####.###.#.#.###.###.#.#.#.#########.#.#.#######.#.#############.#####.#############.###.###.# 
 #.#.....#.....#.#.......#.#.#.#.....#.#...#.....#.#.#.#.#.#.#.#.#.#.#.......#.#.#.....#.#...#...#.#...#...#.#.......#.....# 
 #.###.#######.#######.#######.#.#####.#########.#######.#.#.#.#.#.#.#####.###.###.#####.#.###.###.#######.#.#####.###.###.# 
 #.....#...#.......#.#...#.......#...#.#.#...#.....#.............#.#.....#...#.#...#.....#...#.#.#...........#.#...#.#.#.#.# 
 #.###.###.#####.###.#####.#####.###.#.#.#.###.#####.#.#####.#######.#####.###.###.#.###.#.###.#.###.###.#####.###.#.#.#.### 
 #.#.#.#.......#...#.#...#...#.#.#.#.......#.......#.#...#.....#.....#.#.....#.........#.#...#...#.....#.#.#...#.........#.# 
 ###.#.#####.###.###.#.#######.###.#.###.#####.###.#.#.#.###########.#.#####.###.#.###.###.#####.###.#####.#.#######.#####.# 
 #...#.......#...........#.#.#.#...#.#.#.#.#...#.#.#.#.#...#.#.#.#.#.....#...#.#.#...#.#.....#.#.#.#...#.....#.....#.#.#.#.# 
 ###.#######.#####.#######.#.#.###.###.#.#.###.#.#####.#####.#.#.#.#.###.###.#.#.#########.###.###.#.#######.###.###.#.#.#.# 
 #.....#.#.....#.....#...#...................#...#.....#.........#.....#.#...#.......#.#...#...#.#.....#.#.........#.......# 
 #####.#.###.###.#.###.###.#######.###.#########.#####.#.#.###.#####.###.###.#.#######.#.#.#.###.###.#.#.###.#.#.###.#####.# 
 #.....#.#.....#.#.......#.#...#.#.#.......#.....#.#.#.#.#.#.#.#.#...#...#...#.....#...#.#.#.#.#.....#...#.#.#.#...#...#.#.# 
 #####.#.#####.###.###.#######.#.###.#####.#.#.###.#.#.#.###.###.###.#######.###.#####.#.###.#.#####.#####.#####.#.#.###.### 
 #.#.#...#...#...#.#.#.#.#.......#.#.#...#.#.#.#.....#...#.#.....#...#.#...#.#...#...............#.......#.....#.#.....#...# 
 #.#.#.#.###.#.#####.#.#.###.#####.#.#.#.#.###.###.#.#.###.###.#.#.###.#.#.#.#.#.#.#.#####.#########.#######.#######.###.### 
 #.#...#...#.#...#...#.#.#.....#.....#.#.#...#.....#.#.....#...#.#...#...#...#.#...#...#.#.....#.#...#.#...#.#.........#...# 
 #.###.#####.#.#####.#.#.#.#####.#.###.#.###.#######.#.#######.#.###.#.###.#.#.#.###.###.#######.#.###.#.#.#.#########.#.### 
 #.#...#...#.....#...#.#...#.#.#.#.#...#.....#.....#.#.......#.#.....#.#.#.#.#.#.#...#.....#.#.....#.#...#.#...#.#.#.#.#.#.# 
 #.#.###.###.#.#.#.###.###.#.#.###.###.###.###.#.###.###.###.#.#####.#.#.#########.###.#.###.###.###.#.#####.###.#.#.#.#.#.# 
 #.#...#...#.#.#.#.#.#.#.#.....#...#.....#...#.#...#...#.#...#.....#.#.....#.#.#.......#.#...#.#.#.#.#.#.#.#...#.#.........# 
 #.#.###.#######.#.#.#.#.#####.#######.###.###.###.#.#######.#####.#.###.###.#.#####.#####.###.#.#.#.#.#.#.#.#.#.#.#.#.#.### 
 #...#.....#.#...#...#.....#.....#.#.#...#.#.....#.....#.#.....#.#.#.#.#.#...#...#...........#.#...#.........#.#.#.#.#.#...# 
 #.#####.###.###.#.#.#.#######.###.#.#.#.###.###.#.#.###.###.###.#.###.#.#.#####.#.#####.#####.#.#####.###.#####.#####.##### 
 #.....#.#.....#.#.#.#.#.#.....#.......#.#...#...#.#...#.....#.........#.....#.........#.#.....#.......#.#.#.....#.#.#.....# 
 ###.###.###.###.###.#.#.###.#########.#########.#######.###########.#####.#####.#############.#.###.###.#######.#.#.#####.# 
 #.....#.#...#.....#.#.#...#.#.#.#    Y         a       H           d     c     b          #.....#.#.#.......#...#...#...#.# 
 ###.###.###.###.###.#.###.#.#.#.#                                                         #####.#.###.#.###.#.###.###.###.# 
A......#.................#...#...#                                                         #.#.......#.#.#.....#.#.......#.# 
 ###.###.###.#.#.#.#####.#.#.###.#                                                         #.#####.#########.###.###.#.###.# 
 #...#...#.#.#.#.#...#.#.#.#.#....O                                                        #.....#.....#.#.#...#.#.#.#.#.#.# 
 #.#.#.###.#.#########.#.#.#.#.###                                                         #.#.###.#####.#.#.#.#.#.###.#.#.# 
 #.#.#...#.....#.#.#...#.#.#.....#                                                        P..#.....#.#...#.#.#.#.......#....O
 #.###.###.#.###.#.#.#.#.#.#######                                                         #####.#.#.#.###.###.#.#.###.###.# 
H........#.#.#.#...#.#.#...#.#.#.#                                                         #.#...#.#.....#.......#.#.....#.# 
 #############.#.#####.#####.#.#.#                                                         #.###.#.#.###.###.#.#.###.#####.# 
 #.....#.#.#...#.......#.........#                                                         #...#.#.....#.....#.#.#.........# 
 #.###.#.#.#.#.#.#####.#.#####.###                                                         ###.###################.#####.### 
 #...#.......#...#.........#.....#                                                        L..#...................#...#.....# 
 ###.#####.###.#.###.#.#.#######.#                                                         #.#.#####.#.#.#.#####.#.#####.### 
 #.#...#...#...#...#.#.#.#...#...#                                                         #.#.#...#.#.#.#...#...#.....#.#.# 
 #.###.###.###.#.#####.###.###.###                                                         #.#.#.#####.###.###.#####.#####.# 
I........#.#.#.#.#...#.#.#...#....E                                                        #.#...#...#...#...#.#...#.#.#...# 
 ###.#######.#######.###.###.#####                                                         #.###.###.#########.#.#.###.#.### 
 #.......#...#.#.........#.......#                                                         #.......#.....#.#.#...#..........R
 #####.###.#.#.#.#.#.###.#.#####.#                                                         #.#######.#####.#.###.###.####### 
 #.#.#.#...#.....#.#...#.....#....R                                                        #...#.............#...#.....#....c
 #.#.#########.#####.###.#.#.#.#.#                                                         #.###.###.###.###.###.###.###.### 
J..#.......#.#.#.#...#.#.#.#.#.#.#                                                         #.#...#...#.....#...#.#.#.#.#...# 
 #.#.#.#####.#.#.#####.###########                                                         #####.#.###.#.#.###.#.#.###.#.### 
 #...#...........#...#.#.#.......#                                                        S..#...#.#.#.#.#.#...#.#.#.#.....# 
 #######.#.#.#####.###.#.#####.###                                                         #.###.###.#########.###.#.#.##### 
 #.#...#.#.#.#.........#.........#                                                         #.....#...#.......#.........#.#.# 
 #.#.###########.#####.#.###.###.#                                                         #########.#######.#######.#.#.#.# 
 #.....#...#.#.#.#.#.......#.#.#.#                                                         #.........#.#.#.......#.#.#.#...# 
 #.#.###.#.#.#.#.###.#######.#.#.#                                                         #.#######.#.#.#.#.#.#.#.#######.# 
K..#...#.#.#...#...#.....#.#.#...#                                                        X........#.......#.#.#.........#..P
 #####.#.#.#.#.###.#.#####.#####.#                                                         #.###.#####.###.#####.#.#.#####.# 
 #.#.#...#...#.....#...#.#.#.#.#..B                                                        #.#...#.#.....#.#.....#.#.....#.# 
 #.#.#######.###.#######.#.#.#.#.#                                                         #####.#.###.###.###########.###.# 
 #.#.....#.#...#...#.#.#.......#.#                                                         #...#...#...#...#.#.......#.....# 
 #.###.#.#.#########.#.#.###.#.###                                                         #.#######.#.###.#.#.#.#.######### 
 #.....#...#.#.#...#.#.#.#.#.#...#                                                         #.#...#...#.#...#.#.#.#...#.....# 
 #.#####.###.#.#.###.#.###.#.#.###                                                         #.###.#####.#####.#####.#.#.#.### 
 #.....#.#.#.....#.....#...#.#.#..V                                                        #.#.#...#.#...#.#.#.#...#...#....d
 #####.#.#.###.#.###.###.#.#.#.#.#                                                         #.#.###.#.#####.#.#.#.#####.#.#.# 
L......#.......#.........#...#...#                                                        N....#.#.#.....#.#.#.....#.#.#.#.# 
 #.#.###.#########################                                                         #.###.#.###.###.#.#.#####.#####.# 
 #.#.#.#.#.......................#                                                         #.....................#.#.#...#.# 
 ###.#.#######.#########.###.#.#.#                                                         #.#.#.#.###.###.###.###.#.###.### 
Z..#.#.......#.#.#.....#.#.#.#.#..T                                                        #.#.#.#...#...#.#.#.#.......#...# 
 #.###.###.###.#.#######.#.#.#.#.#                                                         ###########.#####.#####.###.#.#.# 
M......#...#.#.......#...#...#.#.#                                                         #.#...#.#...#...........#.....#.# 
 #.#.###.###.#######.#############                                                         #.###.#.###.###########.#######.# 
 #.#.#.................#.#.#.#...#                                                        G..#.#.....#.#.#...#.#.#...#.#....V
 #######.###############.#.#.###.#                                                         #.#.#.#######.#.###.#.###.#.#.#.# 
 #.....#.#.....................#.#                                                         #...........................#.#.# 
 #.#.#.###.#.#.#####.###.###.#.#.#                                                         #.#.#.#####.#.#.###.###.#.#####.# 
N..#.#.....#.#.#.......#...#.#....C                                                        #.#.#...#...#.#...#.#...#.....#.# 
 #.###.#.#.#####.###.#########.#.#                                                         #.#####.#####.###########.###.#.# 
 #.#...#.#...#.....#...#.......#.#                                                         #.#.#.....#.#.#.#.#.....#...#.#.# 
 #.###.#.#.#.###.###.#######.#####                                                         #.#.#.#.###.#.#.#.#.#####.#.#.### 
 #.#...#.#.#...#.#.....#.#.......#    M         U       I       W   K   J       F     D    #.#...#.#...............#.#.#...# 
 #.#.###.#######.###.###.###.#########.#########.#######.#######.###.###.#######.#####.#########.#.#.#.###.#.###.#.######### 
 #.#.#.......#...#...#.#...#.#...#.#.#.#.........#.#.....#...#.....#.......#.#...#.#...........#.#.#.#...#.#...#.#.....#...# 
 #######.#######.#.#.#.###.###.###.#.#.###.###.###.#.#####.#.#.#.#######.###.###.#.#.#####.#######.#.#.###.#######.#.#.###.# 
 #...........#...#.#.#...#.....#.....#...#.#...#.....#.#.#.#.#.#.#.#.#...#.....#.#.#.....#.#.#.#.#.#.#...#...#.#...#.#.....# 
 #.#.#.#.#.#.###.#####.#######.###.#####.#.#######.#.#.#.#.#.###.#.#.###.#.###.#.#.###.#####.#.#.#####.#######.#.###.#.#.#.# 
 #.#.#.#.#.#.#.....#.....#.#.............#.#...#.#.#.....#.#.#...#.#...#.#.#...#...#...#...........#.......#...#.#.#.#.#.#.# 
 #.#####.#.###.#####.#.###.#.#.#######.###.###.#.#.#.#.###.#.#.###.#.###.#.#.###.#####.###.###########.#.#####.###.#.###.### 
 #.#.#...#.#...#.#.#.#.......#...#.......#.......#.#.#.#.#.#.#.......#...#.#...#...#.#.......#.....#.#.#.......#.......#...# 
 #.#.#.#.#.###.#.#.#######.#.#######.#.#.#####.#.#.#####.#.#.###.#######.#.###.#.#.#.#.###.#.#####.#.###.###.#####.###.#.#.# 
 #.#...#.#...#.#...........#.....#...#.#.#...#.#.#...#...#.#...#.#.......#.#.....#.#.....#.#...#.....#.....#.....#...#.#.#.# 
 #.#.###.#.###.#.#.#.#.#.###.###.#####.#.#.#.###.#.###.###.###.#.###.###.#.#########.#############.#######.###.#######.#.### 
 #.#...#.#.#...#.#.#.#.#.#.....#.....#.#.#.#.....#.......#.#.#.#.#.#.#...#.#...#.#.......................#...#.....#.#.#.#.# 
 #.###.###.#####.###.###.#######.#######.#.###########.###.#.#.#.#.###.#.#.#.#.#.###.#.###.###.###.#.###.#####.#.###.###.#.# 
 #.#...#.......#.#.....#...#.#.#.#.#.#.#.#.......#.......#.#.......#.#.#.#...#.....#.#...#...#.#...#...#.#.....#.#...#.....# 
 #.#####.###.###.###.#.#####.#.#.#.#.#.#.###.#.#.#.#######.#####.###.###.###.###.#####.###########.#####.#.#####.###.#####.# 
 #...#.....#...#.#...#.#.....#.#.#.#.#.#.#.#.#.#.#.#.#.......#.....#.#...#...#.#.#.....#.#.#.#.#.......#.#.#...#.......#...# 
 #.#.###.#######.#.#.###.###.#.###.#.#.#.#.#####.#.#.#####.#.#.###.#.#.#.#####.#.###.#.#.#.#.#.###.#####.#.###.#####.#####.# 
 #.#.#...#.......#.#.#...#.....#...#.......#.....#.....#...#.#...#...#.#.....#.#.#.#.#.....#.#.#.#.#.#.#.#.#...#.#.......#.# 
 #.###.#.#.#####.###.#######.#####.#.###.#.###.#####.#####.#############.#.###.#.#.###.#####.#.#.###.#.#######.#.########### 
 #...#.#.#...#...#...#.#.....#.........#.#.#.#.....#.....#...#...#.......#.#...#.#...#...........#.....#...#...........#.#.# 
 #.###.###.###########.#####.###.###.#.#####.###.###.###.#.###.#######.#.#####.#.#.###.#.#.#.#####.#####.###.#######.###.#.# 
 #.#.#...#.#.#...#.#.#...#.#.......#.#...#.......#.#...#.#.#.....#...#.#.#...........#.#.#.#.........#.#...#.......#.#.#...# 
 #.#.#.#####.###.#.#.###.#.#.###.###.###.#####.###.#.#.###.#.#.#####.#.###.#.###.#####.#######.#.###.#.###.#.###.#####.#.#.# 
 #...#.#...#...#.#.#...#.....#.#.#...#.......#...#.#.#.#.....#.#.#...#.#.#.#.#.....#.........#.#...#...........#.....#...#.# 
 #.#####.#####.#.#.#.###.#.###.#####.#.#######.#.#.#.#.#######.#.#.#.#.#.#.#########.###.#######.#####.#####.#.#.#####.##### 
 #.....#.................#.#.....#...#...#.#.#.#.#...#.#.....#...#.#.....#.#...#.#.#...#.......#.....#...#.#.#.#...........# 
 #.#####.###.###.#####.###.#####.#.#.###.#.#.###.#.#####.#.#.#.###.###.###.###.#.#.#.###########.#.#######.#####.#.#.###.### 
 #.#.#.....#.#...#...#.#.#.#.......#.#.#.#.......#.....#.#.#.#...#...#.#.........#.....#.......#.#.............#.#.#...#...# 
 #.#.#.#####.#######.###.#########.###.#.#.#########.#####.#.#.###.#.#####.#####.#.#.#####.#######.###.#.#.#####.#.#.###.### 
 #.#.....#.....#...#.#.........#.#.#.#.#.#.....#...#.#.#.#.#.....#.#.#.#.#.#...#.#.#.#.#.......#...#...#.#.....#.#.#...#...# 
 #####.#.#.#.###.###.###.#####.#.###.#.#.#.###.#.###.#.#.#.#########.#.#.###.#.#.#.###.#.###.#.#######.#########.#.#.#.#.### 
 #.....#.#.#.#...............#...........#...#.....#.....#...#...........#...#...#.......#...#.......#.........#.#.#.#.#...# 
 ###################################.#########.#########.###.#####.#.#####.###########.##################################### 
                                    b         W         U   S     a X     T           Y                                      
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
