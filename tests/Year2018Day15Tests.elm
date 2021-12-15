module Year2018Day15Tests exposing (..)

import Advent
import Expect
import Grid
import Test exposing (Test)
import Year2018.Day15 as Problem exposing (Entity(..))


readingOrder : Test
readingOrder =
    Test.describe "reading order"
        [ Test.test "given a list of positions it sorts them" <|
            \() ->
                """
#######
#.G.E.#
#E.G.E#
#.G.E.#
#######
"""
                    |> Advent.removeNewlinesAtEnds
                    |> Problem.parse1
                    |> Problem.getNPCs
                    |> List.map Tuple.first
                    |> Problem.sortByReadingOrder
                    {-
                       #######
                       #.1.2.#
                       #3.4.5#
                       #.6.7.#
                       #######
                    -}
                    |> Expect.equal
                        [ ( 2, 1 )
                        , ( 4, 1 )
                        , ( 1, 2 )
                        , ( 3, 2 )
                        , ( 5, 2 )
                        , ( 2, 3 )
                        , ( 4, 3 )
                        ]
        ]


outcome : Test
outcome =
    let
        cases : List ( String, ( Int, List Int ), Int )
        cases =
            [ ( "main example", ( 47, [ 200, 131, 59, 200 ] ), 27730 )
            , ( "summarized example 1", ( 37, [ 200, 197, 185, 200, 200 ] ), 36334 )
            , ( "summarized example 2", ( 46, [ 164, 197, 200, 98, 200 ] ), 39514 )
            , ( "summarized example 3", ( 35, [ 200, 98, 200, 95, 200 ] ), 27755 )
            , ( "summarized example 4", ( 54, [ 200, 98, 38, 200 ] ), 28944 )
            , ( "summarized example 5", ( 20, [ 137, 200, 200, 200, 200 ] ), 18740 )
            ]

        testCase : ( String, ( Int, List Int ), Int ) -> Test
        testCase ( label, ( fullRounds, hitpoints ), expectedOutcome ) =
            Test.test label <|
                \() ->
                    Problem.outcomeScore
                        { fullRounds = fullRounds
                        , hitpoints = hitpoints
                        }
                        |> Expect.equal expectedOutcome
    in
    Test.describe "outcome" <|
        List.map testCase cases


isEnd : Test
isEnd =
    let
        cases : List ( String, List Entity, Bool )
        cases =
            [ ( "single goblin", [ Goblin 200 ], True )
            , ( "single elf", [ Elf 1 ], True )
            , ( "two goblins", [ Goblin 10, Goblin 20 ], True )
            , ( "two elves", [ Elf 10, Elf 20 ], True )
            , ( "mixed", [ Goblin 10, Elf 20 ], False )
            , ( "empty", [], True )
            ]

        testCase : ( String, List Entity, Bool ) -> Test
        testCase ( label, entities, expectedOutput ) =
            Test.test label <|
                \() ->
                    Problem.isEnd entities
                        |> Expect.equal expectedOutput
    in
    Test.describe "isEnd" <|
        List.map testCase cases
