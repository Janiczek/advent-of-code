module Year2018.Day07 exposing (Dependency, Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)
import Dict.Extra
import List.Extra
import Set exposing (Set)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List Dependency


type alias Input2 =
    List Dependency


type alias Output1 =
    String


type alias Output2 =
    Int


type alias Dependency =
    { first : Char
    , second : Char
    }



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> List.map parseDependency


parseDependency : String -> Dependency
parseDependency string =
    case String.words string of
        [ _, first, _, _, _, _, _, second, _, _ ] ->
            { first = String.uncons first |> Advent.unsafeMaybe |> Tuple.first
            , second = String.uncons second |> Advent.unsafeMaybe |> Tuple.first
            }

        _ ->
            Debug.todo "wrong input 1"


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 deps =
    let
        all : Set Char
        all =
            deps
                |> List.concatMap (\dep -> [ dep.first, dep.second ])
                |> Set.fromList

        inits : List Char
        inits =
            step [] deps

        last : List Char
        last =
            Set.diff all (Set.fromList inits)
                |> Set.toList
    in
    String.fromList (inits ++ last)


step : List Char -> List Dependency -> List Char
step done deps =
    if List.isEmpty deps then
        done

    else
        let
            ready : List Char
            ready =
                findReady deps |> List.sort

            chosen : Char
            chosen =
                List.head ready
                    |> Advent.unsafeMaybe

            newDone : List Char
            newDone =
                done ++ [ chosen ]

            newDeps : List Dependency
            newDeps =
                deps
                    |> List.filter (\{ first } -> first /= chosen)
        in
        step newDone newDeps


findReady : List Dependency -> List Char
findReady deps =
    let
        firsts : Set Char
        firsts =
            Set.fromList (List.map .first deps)

        seconds : Set Char
        seconds =
            Set.fromList (List.map .second deps)
    in
    Set.diff firsts seconds
        |> Set.toList


workers : Int
workers =
    5


constant2 : Int
constant2 =
    60


compute2 : Input2 -> Output2
compute2 deps =
    let
        doneNormally : String
        doneNormally =
            compute1 deps

        lastOfNormal : Char
        lastOfNormal =
            doneNormally
                |> String.toList
                |> List.Extra.last
                |> Advent.unsafeMaybe

        newDeps : List Dependency
        newDeps =
            deps ++ [ Dependency lastOfNormal '.' ]
    in
    step2 0 [] newDeps []


step2 : Int -> List Char -> List Dependency -> List ( Int, Char ) -> Int
step2 second done deps work =
    if List.isEmpty deps then
        second

    else
        let
            ( doneNow, remainingWork ) =
                work
                    |> List.partition (\( secDone, _ ) -> secDone <= second)

            doneNowChars : List Char
            doneNowChars =
                doneNow
                    |> List.map Tuple.second
                    |> List.sort

            newDone : List Char
            newDone =
                done ++ doneNowChars

            doneNowSet : Set Char
            doneNowSet =
                Set.fromList doneNowChars

            availableWorkers : Int
            availableWorkers =
                workers - List.length remainingWork

            newDeps : List Dependency
            newDeps =
                deps
                    |> List.filter (\{ first } -> not (Set.member first doneNowSet))

            beingWorkedOn : Set Char
            beingWorkedOn =
                work
                    |> List.map Tuple.second
                    |> Set.fromList

            readyForWork : List Char
            readyForWork =
                findReady newDeps
                    |> List.filter (\c -> not (Set.member c beingWorkedOn))
                    |> List.sort

            newWork : List ( Int, Char )
            newWork =
                remainingWork
                    ++ (readyForWork
                            |> List.take availableWorkers
                            |> List.map (\c -> ( second + duration c, c ))
                       )

            newSecond : Int
            newSecond =
                newWork
                    |> List.map Tuple.first
                    |> List.minimum
                    |> Maybe.withDefault second
        in
        step2 newSecond newDone newDeps newWork


constant : Int
constant =
    Char.toCode 'A' - 1


duration : Char -> Int
duration char =
    Char.toCode char - constant + constant2



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "example"
        """Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin."""
        [ Dependency 'C' 'A'
        , Dependency 'C' 'F'
        , Dependency 'A' 'B'
        , Dependency 'A' 'D'
        , Dependency 'B' 'E'
        , Dependency 'D' 'E'
        , Dependency 'F' 'E'
        ]
        "CABDFE"
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
Step L must be finished before step A can begin.
Step P must be finished before step F can begin.
Step V must be finished before step U can begin.
Step F must be finished before step S can begin.
Step A must be finished before step J can begin.
Step R must be finished before step K can begin.
Step Z must be finished before step T can begin.
Step G must be finished before step W can begin.
Step H must be finished before step K can begin.
Step T must be finished before step U can begin.
Step K must be finished before step B can begin.
Step C must be finished before step Y can begin.
Step W must be finished before step N can begin.
Step E must be finished before step M can begin.
Step N must be finished before step J can begin.
Step B must be finished before step S can begin.
Step O must be finished before step D can begin.
Step X must be finished before step D can begin.
Step M must be finished before step Q can begin.
Step S must be finished before step J can begin.
Step U must be finished before step Y can begin.
Step I must be finished before step J can begin.
Step D must be finished before step J can begin.
Step Q must be finished before step Y can begin.
Step J must be finished before step Y can begin.
Step Z must be finished before step D can begin.
Step K must be finished before step E can begin.
Step U must be finished before step J can begin.
Step I must be finished before step Y can begin.
Step A must be finished before step B can begin.
Step B must be finished before step Q can begin.
Step Z must be finished before step S can begin.
Step F must be finished before step E can begin.
Step B must be finished before step I can begin.
Step C must be finished before step S can begin.
Step O must be finished before step S can begin.
Step V must be finished before step O can begin.
Step C must be finished before step B can begin.
Step G must be finished before step M can begin.
Step O must be finished before step Y can begin.
Step H must be finished before step N can begin.
Step D must be finished before step Y can begin.
Step Z must be finished before step O can begin.
Step K must be finished before step W can begin.
Step M must be finished before step Y can begin.
Step O must be finished before step J can begin.
Step P must be finished before step E can begin.
Step C must be finished before step Q can begin.
Step I must be finished before step D can begin.
Step F must be finished before step I can begin.
Step W must be finished before step B can begin.
Step W must be finished before step M can begin.
Step N must be finished before step D can begin.
Step Z must be finished before step M can begin.
Step M must be finished before step U can begin.
Step R must be finished before step I can begin.
Step S must be finished before step Y can begin.
Step L must be finished before step B can begin.
Step S must be finished before step D can begin.
Step R must be finished before step G can begin.
Step U must be finished before step D can begin.
Step C must be finished before step N can begin.
Step R must be finished before step T can begin.
Step K must be finished before step U can begin.
Step W must be finished before step E can begin.
Step H must be finished before step E can begin.
Step X must be finished before step M can begin.
Step G must be finished before step I can begin.
Step C must be finished before step U can begin.
Step N must be finished before step B can begin.
Step X must be finished before step S can begin.
Step G must be finished before step H can begin.
Step T must be finished before step X can begin.
Step P must be finished before step N can begin.
Step B must be finished before step Y can begin.
Step S must be finished before step Q can begin.
Step C must be finished before step E can begin.
Step F must be finished before step D can begin.
Step H must be finished before step J can begin.
Step B must be finished before step U can begin.
Step B must be finished before step J can begin.
Step P must be finished before step I can begin.
Step N must be finished before step X can begin.
Step M must be finished before step J can begin.
Step X must be finished before step I can begin.
Step L must be finished before step P can begin.
Step T must be finished before step B can begin.
Step T must be finished before step K can begin.
Step D must be finished before step Q can begin.
Step W must be finished before step X can begin.
Step A must be finished before step Y can begin.
Step G must be finished before step D can begin.
Step R must be finished before step Z can begin.
Step U must be finished before step Q can begin.
Step G must be finished before step O can begin.
Step G must be finished before step Q can begin.
Step G must be finished before step Y can begin.
Step P must be finished before step Y can begin.
Step I must be finished before step Q can begin.
Step F must be finished before step C can begin.
Step L must be finished before step K can begin.
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
