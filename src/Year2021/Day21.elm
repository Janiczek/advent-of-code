module Year2021.Day21 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    ( Int, Int )


type alias Input2 =
    ( Int, Int )


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    let
        parseLine line =
            line
                |> String.right 1
                |> String.toInt
                |> Advent.unsafeMaybe "parseLine"
    in
    case List.map parseLine (String.lines string) of
        [ a, b ] ->
            ( a, b )

        _ ->
            Debug.todo "parse1"


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


type alias State1 =
    { n : Int
    , rolls : Int
    , space : Int
    , otherSpace : Int
    , score : Int
    , otherScore : Int
    }


biasedModBy : Int -> Int -> Int
biasedModBy m n =
    let
        newN =
            modBy m n
    in
    if newN == 0 then
        m

    else
        newN


compute1 : Input1 -> Output1
compute1 ( p1Space, p2Space ) =
    let
        go : State1 -> Int
        go s =
            let
                nextRolls =
                    s.rolls + 3

                nextN =
                    (s.n + 3) |> biasedModBy 100

                rolledSum =
                    List.range (s.n + 1) (s.n + 3)
                        |> List.map (biasedModBy 100)
                        |> List.sum

                nextSpace =
                    (s.space + rolledSum)
                        |> biasedModBy 10

                nextScore =
                    s.score + nextSpace
            in
            if nextScore >= 1000 then
                s.otherScore * nextRolls

            else
                go
                    { n = nextN
                    , rolls = nextRolls

                    -- we're switching
                    , space = s.otherSpace
                    , otherSpace = nextSpace
                    , score = s.otherScore
                    , otherScore = nextScore
                    }
    in
    go
        { n = 0
        , rolls = 0
        , space = p1Space
        , otherSpace = p2Space
        , score = 0
        , otherScore = 0
        }


compute2 : Input2 -> Output2
compute2 ( p1Space, p2Space ) =
    -1



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "example"
        """Player 1 starting position: 4
Player 2 starting position: 8"""
        Nothing
        739785
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    [ Test "example"
        """Player 1 starting position: 4
Player 2 starting position: 8"""
        Nothing
        444356092776315
    ]



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
Player 1 starting position: 7
Player 2 starting position: 4
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
