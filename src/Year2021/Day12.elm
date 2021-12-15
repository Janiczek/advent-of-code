module Year2021.Day12 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)
import Dict.Extra as Dict
import Graph exposing (Edge, Graph)
import Set exposing (Set)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    Graph String ()


type alias Input2 =
    Graph String ()


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> List.concatMap parseLine
        |> Graph.fromVerticesAndEdges []


parseLine : String -> List (Edge String ())
parseLine line =
    case String.split "-" line of
        [ from, to ] ->
            [ { from = from
              , to = to
              , data = ()
              }
            , { from = to
              , to = from
              , data = ()
              }
            ]

        _ ->
            Debug.todo "parseLine bug"


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


type alias Path =
    List String


type alias Todo =
    { current : String
    , pathSoFar : List String
    , visitedSmall : Dict String Int
    }


type alias State =
    { foundPaths : Set Path
    , todos : List Todo
    }


start : String
start =
    "start"


end : String
end =
    "end"


init : State
init =
    { todos =
        [ { current = start
          , pathSoFar = []
          , visitedSmall = Dict.empty
          }
        ]
    , foundPaths = Set.empty
    }


findAllPaths : Int -> Graph String () -> State -> Set Path
findAllPaths singleSmallN graph state =
    case state.todos of
        [] ->
            state.foundPaths

        { current, pathSoFar, visitedSmall } :: rest ->
            let
                newPathSoFar =
                    current :: pathSoFar
            in
            let
                newVisitedSmall =
                    if String.all Char.isLower current then
                        Dict.update
                            current
                            (\maybeCount ->
                                case maybeCount of
                                    Nothing ->
                                        Just 1

                                    Just n ->
                                        Just (n + 1)
                            )
                            visitedSmall

                    else
                        visitedSmall

                maxCountBeforeAdding =
                    if singleSmallN == 1 then
                        0

                    else if Dict.any (\_ c -> c == singleSmallN) newVisitedSmall then
                        singleSmallN - 2

                    else
                        singleSmallN - 1
            in
            let
                newTodos =
                    Graph.outgoingEdges current graph
                        |> List.filter
                            (\room ->
                                (room /= start)
                                    && (String.any Char.isUpper room
                                            || (Dict.get room newVisitedSmall
                                                    |> Maybe.withDefault 0
                                                    |> (\c -> c <= maxCountBeforeAdding)
                                               )
                                       )
                            )
                        |> List.map
                            (\room ->
                                { current = room
                                , pathSoFar = newPathSoFar
                                , visitedSmall = newVisitedSmall
                                }
                            )
            in
            findAllPaths
                singleSmallN
                graph
                (if current == end then
                    { state
                        | foundPaths = Set.insert (List.reverse newPathSoFar) state.foundPaths
                        , todos = rest
                    }

                 else
                    { state | todos = newTodos ++ rest }
                )


compute1 : Input1 -> Output1
compute1 graph =
    findAllPaths 1 graph init
        |> Set.size


compute2 : Input2 -> Output2
compute2 graph =
    findAllPaths 2 graph init
        |> Set.size



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "example"
        """start-A
start-b
A-c
A-b
b-d
A-end
b-end"""
        Nothing
        10
    , Test "larger example"
        """dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"""
        Nothing
        19
    , Test "even larger example"
        """fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"""
        Nothing
        226
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    [ Test "example"
        """start-A
start-b
A-c
A-b
b-d
A-end
b-end"""
        Nothing
        36
    , Test "larger example"
        """dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"""
        Nothing
        103
    , Test "even larger example"
        """fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"""
        Nothing
        3509
    ]



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
vp-BY
ui-oo
kk-IY
ij-vp
oo-start
SP-ij
kg-uj
ij-UH
SP-end
oo-IY
SP-kk
SP-vp
ui-ij
UH-ui
ij-IY
start-ui
IY-ui
uj-ui
kk-oo
IY-start
end-vp
uj-UH
ij-kk
UH-end
UH-kk
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
