module Year2017.Day24 exposing (..)

import Advent exposing (Test)
import Dict exposing (Dict)


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


type alias Pipe =
    ( Int, Int )


type alias Connection =
    List Pipe


type alias Input =
    List Pipe


type alias Output =
    Int


parse : String -> Input
parse input =
    input
        |> String.lines
        |> List.map parsePipe


parsePipe : String -> Pipe
parsePipe string =
    case String.split "/" string of
        [ a, b ] ->
            ( Advent.toInt a, Advent.toInt b )

        _ ->
            Debug.crash "wrong input!"


connectablePipes : Pipe -> List Pipe -> List Pipe
connectablePipes pipe allPipes =
    allPipes
        |> List.filter (\p -> p /= pipe && canConnect p pipe)


canConnect : Pipe -> Pipe -> Bool
canConnect ( p1start, p1end ) ( p2start, p2end ) =
    (p1start == p2start)
        || (p1start == p2end)
        || (p1end == p2start)
        || (p1end == p2end)


compute1 : Input -> Output
compute1 allPipes =
    let
        possibleConnections : Dict Pipe (List Pipe)
        possibleConnections =
            allPipes
                |> List.map (\p -> ( p, connectablePipes p allPipes ))
                |> Dict.fromList
    in
        findAllConnections possibleConnections allPipes
            |> List.map (\c -> ( c, strength c ))
            |> List.sortBy Tuple.second
            |> List.reverse
            |> List.head
            |> Advent.unsafeMaybe
            |> Tuple.second


findAllConnections : Dict Pipe (List Pipe) -> List Pipe -> List Connection
findAllConnections possibleConnections allPipes =
    let
        possibleFirstPipes : List Pipe
        possibleFirstPipes =
            allPipes
                |> List.filter hasZero

        connections : List Connection
        connections =
            possibleFirstPipes
                |> List.map List.singleton
    in
        connections
            |> List.concatMap (findNextSteps possibleConnections allPipes)


findNextSteps : Dict Pipe (List Pipe) -> List Pipe -> Connection -> List Connection
findNextSteps possibleConnections allPipes connection =
    let
        lastPipe : Pipe
        lastPipe =
            connection
                |> List.reverse
                |> List.head
                |> Advent.unsafeMaybe

        ( lastStart, lastEnd ) =
            lastPipe

        maybeNextToLastPipe : Maybe Pipe
        maybeNextToLastPipe =
            connection
                |> List.reverse
                |> List.drop 1
                |> List.head

        unusedEndOfLastPipe : Int
        unusedEndOfLastPipe =
            maybeNextToLastPipe
                |> Maybe.map
                    (\( start, end ) ->
                        if start == lastStart then
                            lastEnd
                        else if start == lastEnd then
                            lastStart
                        else if end == lastStart then
                            lastEnd
                        else
                            lastStart
                    )
                -- the whole bridge starts with 0, we can't use that
                |> Maybe.withDefault
                    (if lastStart == 0 then
                        lastEnd
                     else
                        lastStart
                    )

        nextPipes : List Pipe
        nextPipes =
            possibleConnections
                |> Dict.get lastPipe
                |> Advent.unsafeMaybe
                |> List.filter (\p -> not (List.member p connection))
                |> List.filter
                    (\( start, end ) ->
                        (start == unusedEndOfLastPipe)
                            || (end == unusedEndOfLastPipe)
                    )

        nextConnections : List Connection
        nextConnections =
            nextPipes
                |> List.map
                    (\nextPipe ->
                        connection ++ [ nextPipe ]
                    )
    in
        if nextPipes == [] then
            [ connection ]
        else
            connection
                :: (nextConnections
                        |> List.concatMap (findNextSteps possibleConnections allPipes)
                   )


hasZero : Pipe -> Bool
hasZero ( start, end ) =
    start == 0 || end == 0


strength : Connection -> Int
strength connection =
    connection
        |> List.map (\( pipeStart, pipeEnd ) -> pipeStart + pipeEnd)
        |> List.sum


compute2 : Input -> Output
compute2 allPipes =
    let
        possibleConnections : Dict Pipe (List Pipe)
        possibleConnections =
            allPipes
                |> List.map (\p -> ( p, connectablePipes p allPipes ))
                |> Dict.fromList

        allConnections : List Connection
        allConnections =
            findAllConnections possibleConnections allPipes

        lengthOfLongest : Int
        lengthOfLongest =
            allConnections
                |> List.map List.length
                |> List.maximum
                |> Advent.unsafeMaybe

        longestConnections : List Connection
        longestConnections =
            allConnections
                |> List.filter (\c -> List.length c == lengthOfLongest)
    in
        longestConnections
            |> List.map (\c -> ( c, strength c ))
            |> List.sortBy Tuple.second
            |> List.reverse
            |> List.head
            |> Advent.unsafeMaybe
            |> Tuple.second


tests1 : List (Test Input Output)
tests1 =
    [ Test "example"
        """0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10"""
        [ ( 0, 2 )
        , ( 2, 2 )
        , ( 2, 3 )
        , ( 3, 4 )
        , ( 3, 5 )
        , ( 0, 1 )
        , ( 10, 1 )
        , ( 9, 10 )
        ]
        31
    ]


tests2 : List (Test Input Output)
tests2 =
    []


input : String
input =
    """14/42
2/3
6/44
4/10
23/49
35/39
46/46
5/29
13/20
33/9
24/50
0/30
9/10
41/44
35/50
44/50
5/11
21/24
7/39
46/31
38/38
22/26
8/9
16/4
23/39
26/5
40/40
29/29
5/20
3/32
42/11
16/14
27/49
36/20
18/39
49/41
16/6
24/46
44/48
36/4
6/6
13/6
42/12
29/41
39/39
9/3
30/2
25/20
15/6
15/23
28/40
8/7
26/23
48/10
28/28
2/13
48/14"""
