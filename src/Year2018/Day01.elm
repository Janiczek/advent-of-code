module Year2018.Day01 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Set exposing (Set)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List Int


type alias Input2 =
    List Int


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> List.map Advent.unsafeToInt


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    List.sum input


compute2 : Input2 -> Output2
compute2 input =
    let
        process : Int -> ( Int, Set Int, Maybe Int ) -> ( Int, Set Int, Maybe Int )
        process line ( current, seen, done ) =
            case done of
                Nothing ->
                    let
                        new : Int
                        new =
                            current + line
                    in
                    if Set.member new seen then
                        ( 0, seen, Just new )

                    else
                        ( new, Set.insert new seen, Nothing )

                Just _ ->
                    ( current, seen, done )

        processManyTimes : ( Int, Set Int, Maybe Int ) -> Int
        processManyTimes ( current, seen, done ) =
            case done of
                Nothing ->
                    let
                        processed : ( Int, Set Int, Maybe Int )
                        processed =
                            input
                                |> List.foldl process ( current, seen, done )
                    in
                    processManyTimes processed

                Just result ->
                    result
    in
    processManyTimes ( 0, Set.singleton 0, Nothing )



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    []


tests2 : List (Test Input2 Output2)
tests2 =
    [ Test "example"
        "+1\n-1"
        Nothing
        0
    , Test "example 2"
        "+3\n+3\n+4\n-2\n-4"
        Nothing
        10
    , Test "example 3"
        "-6\n+3\n+8\n+5\n-6"
        Nothing
        5
    ]



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
-10
+6
-1
-18
-19
+8
-18
-9
-9
+8
+13
+6
-11
+19
-15
-9
-1
+5
+14
-7
+18
-10
+16
+18
+9
-1
+16
-12
-7
-20
+14
-18
+16
-2
-6
-22
+3
+3
-20
+16
+8
-15
-12
-12
-6
-16
-18
-15
-1
+9
+11
+12
-17
-3
+12
+16
+9
+11
+19
+18
+9
+19
-13
+10
+12
+17
-14
-2
+10
-2
+12
+18
-7
+2
+12
-8
-19
+16
+14
+8
-3
+14
+17
-8
-17
+18
-7
-18
-16
+7
+14
-3
-13
+20
+13
+15
+19
+11
+13
-19
+5
+11
+7
+9
+3
+14
-1
+6
-15
-8
-2
+7
-6
-6
-2
+5
+10
-16
-7
-16
-2
+16
-3
+1
+5
+11
-1
+19
+16
+18
-3
+5
+9
-18
-9
-1
+15
+19
-16
-11
+16
+13
+5
+17
+12
-10
+14
+10
+17
+6
-13
-9
+7
-15
-14
-20
+18
+7
-1
-5
+9
+5
+10
-5
+21
+4
+12
+17
+19
-9
-12
-14
-7
+3
-4
-3
-11
-2
-7
-19
-17
+7
-10
-1
-3
+5
-10
+4
-12
+5
+17
+21
+18
-2
-9
-10
-21
-5
+20
-19
-5
+14
+17
+17
+20
+17
-18
+19
-2
+8
+17
-12
+14
+12
+9
-16
-8
+2
+9
+11
+9
-17
-16
+15
+10
+3
+14
+6
+10
-2
+13
-19
-14
+10
+5
-14
-2
-6
+9
-7
+1
-2
-16
-7
+5
-4
-18
+2
+6
-4
-19
+1
+20
-12
+17
+18
+3
+19
+20
+9
+17
-15
-7
-3
+11
+13
+10
-15
-3
+19
-21
+1
+16
+15
-5
-3
+15
+4
-6
+4
+11
+19
+2
-8
-7
+14
-19
-6
+9
+6
-17
+10
+5
-20
+19
+16
+16
+21
-2
+15
+18
-8
-19
+3
-1
+28
+7
+5
-6
-10
+5
-17
+25
-5
+11
+5
+2
+16
+12
-15
-18
+8
+5
+2
-6
+7
-19
-6
+23
-32
+17
+36
+17
+20
-3
+5
+6
-14
+1
-3
+17
+10
-13
+20
+18
-9
-11
+14
-10
+17
+10
+14
+4
+12
+7
-2
-13
+14
-8
+17
+9
-1
+2
+11
+2
-6
-5
+7
-18
+13
-18
-17
-9
+12
+17
-13
+11
-4
+22
-21
+19
+5
+23
-15
+27
+18
+6
+16
+12
+5
-2
+1
-18
-7
-18
-14
+16
+19
+15
-1
+18
+10
-5
-20
+13
+8
+10
+21
+1
+6
+12
-5
+19
+15
-13
-10
-1
+20
+2
-14
+8
+22
+1
-12
-8
+26
+20
-2
-3
+22
+16
+1
+20
-7
-11
-12
-6
-9
-17
-7
+18
-14
+22
-9
+14
-37
+7
-16
+13
-25
-8
-2
+27
+1
+72
+11
+14
+1
+16
+1
-24
+22
-61
-62
+5
-40
+14
-31
+20
+6
+25
-27
-29
-9
-14
-18
+5
-43
+13
+4
+55
-61
-19
-24
-16
-23
-24
-6
-7
+5
-12
+18
-13
-4
-14
-13
-18
-12
+1
+24
-18
+2
-20
-21
+4
-16
+22
+3
-11
-5
+10
-1
+5
-27
-21
+19
-15
-10
+29
+28
+3
+18
+8
+6
-1
-3
+14
+9
-10
-29
+9
-14
-9
-22
-1
+13
+22
+19
-4
-2
-72
+13
+115
+77
-61
+401
+75361
-11
+2
-19
-6
-14
-20
+16
+19
-10
+5
+7
-20
-11
-14
-2
+19
-7
+29
+16
-4
+11
-5
-3
+14
+13
-10
+13
+9
+19
+7
+10
+14
+16
+12
-8
+9
+16
-8
+6
+19
-14
+4
-6
+17
+1
+8
+18
+6
-11
-8
+10
+10
+8
+14
-10
-2
-16
-12
+9
-17
-14
+8
-11
-3
-1
-8
-17
+1
+1
+14
+18
-10
-14
-14
+2
+11
+2
-4
-1
-17
+19
-11
-12
+8
-5
-2
-5
-24
-1
-1
+3
-13
+9
+19
+17
-18
-7
-2
+11
-10
+15
+13
-5
-11
+18
+25
+22
+10
-7
+5
+4
-3
+14
-5
+16
-14
+15
+22
-19
+16
-3
-3
+15
+19
+16
-13
+6
+4
+6
-18
-5
+9
-15
+7
-12
+3
-18
-7
-3
-15
-12
+13
-14
-7
-5
+14
+16
+12
+17
+3
+2
+8
+6
+17
-11
-2
+5
-1
+6
-13
+18
+6
-12
+7
+18
-2
-5
-4
-3
+6
-7
+12
+11
+19
-16
+13
+11
+14
+8
+8
-9
+8
+6
+15
+7
+17
+13
-9
-15
+1
-15
+3
+17
-15
+17
-15
-1
+15
+11
+4
+1
+13
+14
+10
+15
+4
+16
-13
+9
+14
+16
+8
+9
+4
+1
+6
+10
+16
+20
+15
+16
+14
+3
-7
+6
+18
-8
-13
-18
-12
+10
+13
-3
+9
-18
-8
-12
+2
+6
+19
-5
+1
+14
+20
+2
+11
+3
-6
-11
+16
+2
-13
-12
-19
-19
+2
-12
-5
-8
-2
-15
-18
+15
-11
-12
-8
-9
-1
-11
-13
+11
-17
+18
-9
-6
-22
-13
-4
-16
-19
-17
+4
+19
+3
+12
+1
-18
+8
+16
+4
-5
+14
+16
-2
-4
+13
-14
+17
+20
+18
-19
-11
+18
-9
-14
+13
+2
-19
-8
-18
-14
-1
-18
+10
-2
+3
+11
+9
-1
+6
+17
-15
-9
+10
+15
-19
-11
-17
-6
+8
-11
-10
-1
-1
-16
-13
-14
-5
+8
-18
-6
+14
-12
-15
-6
-16
+1
-14
+5
-16
+9
-4
+17
+7
+13
+8
-3
+20
-7
-3
-19
+16
+15
-11
-15
+2
+3
-9
-7
-17
+4
-1
+5
+8
+6
-1
+15
+5
+8
+4
+6
-8
+6
+13
-4
+1
+19
+5
+3
+12
-3
-19
-15
+19
-15
-2
+1
+5
-15
-25
-24
-20
+18
-76214
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
