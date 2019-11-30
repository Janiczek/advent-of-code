module Year2015.Day20 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Arithmetic
import List.Extra



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
    String.toInt string
        |> Maybe.withDefault -1


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    compute1Help input 1


compute1Help : Int -> Int -> Int
compute1Help input n =
    if presentsAtHouse n >= input then
        n

    else
        compute1Help input (n + 1)


presentsAtHouse : Int -> Int
presentsAtHouse n =
    Arithmetic.divisors n
        |> List.sum
        |> (*) 10


compute2 : Input2 -> Output2
compute2 input =
    compute2Help input 1


compute2Help : Int -> Int -> Int
compute2Help input n =
    if presentsAtHouse2 n >= input then
        n

    else
        compute2Help input (n + 1)


divisors2 : Int -> List Int
divisors2 n =
    let
        f ( p, e ) =
            List.concatMap (\a -> List.map (\x -> p ^ x * a) (List.range 0 e))
    in
    Arithmetic.primeExponents n
        |> List.foldr f [ 1 ]
        |> List.sort
        |> List.Extra.dropWhile (\d -> d * 50 < n)


presentsAtHouse2 : Int -> Int
presentsAtHouse2 n =
    divisors2 n
        |> List.sum
        |> (*) 11



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example"
        "input"
        -1
        -1
     -}
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
33100000
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
