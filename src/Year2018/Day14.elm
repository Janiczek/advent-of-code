module Year2018.Day14 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Array exposing (Array)



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
    step (input + 10) start
        |> output


compute2 : Input2 -> Output2
compute2 input =
    Debug.todo "done in Python :("


output : String -> Int
output string =
    String.right 10 string
        |> Advent.unsafeToInt


step : Int -> ( String, Int, Int ) -> String
step neededRecipes ( state, i1, i2 ) =
    let
        length =
            String.length state
    in
    if length >= neededRecipes then
        trim neededRecipes state

    else
        let
            n1 : Int
            n1 =
                state
                    |> String.slice i1 (i1 + 1)
                    |> Advent.unsafeToInt

            n2 : Int
            n2 =
                state
                    |> String.slice i2 (i2 + 1)
                    |> Advent.unsafeToInt

            newNs : String
            newNs =
                new n1 n2

            newNsLength : Int
            newNsLength =
                String.length newNs

            newState : String
            newState =
                state ++ newNs

            newLength : Int
            newLength =
                length + newNsLength

            newI1 : Int
            newI1 =
                (1 + n1 + i1) |> modBy newLength

            newI2 : Int
            newI2 =
                (1 + n2 + i2) |> modBy newLength
        in
        step neededRecipes ( newState, newI1, newI2 )


get : Int -> Array Int -> Int
get index array =
    Array.get index array
        |> Advent.unsafeMaybe


getFromEnd : Int -> Array Int -> Int
getFromEnd index array =
    -- getFromEnd 0 [1,2,3] == 3
    -- getFromEnd -1 [1,2,3] == 2
    let
        length =
            Array.length array
    in
    Array.get (length - index - 1) array
        |> Advent.unsafeMaybe


last : Int -> Array Int -> Array Int
last num array =
    let
        length =
            Array.length array
    in
    Array.slice (length - num) length array


butLast : Int -> Array Int -> Array Int
butLast num array =
    Array.slice 0 (Array.length array - num) array


trim : Int -> String -> String
trim neededLength state =
    let
        length =
            String.length state

        toTrim =
            length - neededLength
    in
    String.dropRight toTrim state


new : Int -> Int -> String
new n1 n2 =
    String.fromInt (n1 + n2)


start : ( String, Int, Int )
start =
    ( "37", 0, 1 )



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    {-
       [ Test "example 1" "9" 9 "5158916779"
       , Test "example 2" "5" 5 "0124515891"
       , Test "example 3" "18" 18 "9251071085"
       , Test "example 4" "2018" 2018 "5941429882"
       ]
    -}
    []


tests2 : List (Test Input2 Output2)
tests2 =
    {-
       [ Test "example 1" "51589" (Array.fromList [ 5, 1, 5, 8, 9 ]) 9
       , Test "example 2" "01245" (Array.fromList [ 0, 1, 2, 4, 5 ]) 5
       , Test "example 3" "92510" (Array.fromList [ 9, 2, 5, 1, 0 ]) 18
       , Test "example 4" "59414" (Array.fromList [ 5, 9, 4, 1, 4 ]) 2018
       ]
    -}
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
503761
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
