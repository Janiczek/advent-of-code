module Year2016.Day19 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import List.Zipper as Zipper exposing (Zipper)
import Set exposing (Set)



{- part 2 wrong answers:
   754487
   1173647
-}
-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    Int


type alias Input2 =
    Int


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    Advent.unsafeToInt string


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    Zipper.fromCons 1 (List.range 2 input)
        |> go1


go1 : Zipper Int -> Int
go1 ps =
    if Zipper.isFirst ps && Zipper.isLast ps then
        Zipper.current ps

    else
        go1
            (ps
                |> removeNext
                |> goToNext
            )


removeNext : Zipper Int -> Zipper Int
removeNext ps =
    if Zipper.isLast ps then
        -- [1,2] 3 [] --> [2] 3 []
        Zipper.mapBefore (List.drop 1) ps

    else
        -- [1,2] 3 [4,5] --> [1,2] 3 [5]
        Zipper.mapAfter (List.drop 1) ps


goToNext : Zipper Int -> Zipper Int
goToNext ps =
    if Zipper.isLast ps then
        -- [1,2] 3 [] --> [] 1 [2,3]
        Zipper.first ps

    else
        -- [1,2] 3 [4,5] -> [1,2,3] 4 [5]
        ps
            |> Zipper.next
            -- impossible:
            |> Maybe.withDefault ps


compute2 : Input2 -> Output2
compute2 input =
    -1



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "example" "5" (Just 5) 3
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    [ Test "example" "5" (Just 5) 2
    , Test "example" "8" (Just 8) 7
    ]



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
3017957
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
