module Year2019.Day04 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import List.Extra
import Seq



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
    case String.split "-" string of
        [ a, b ] ->
            ( Advent.unsafeToInt a
            , Advent.unsafeToInt b
            )

        _ ->
            Debug.todo "what"


toChars : Int -> List Char
toChars int =
    int
        |> String.fromInt
        |> String.toList


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


twoOrMore : List Char -> Bool
twoOrMore digits =
    List.Extra.group digits
        |> List.map (nonemptyListToList >> List.length)
        |> List.filter (\count -> count >= 2)
        |> List.length
        |> (/=) 0


atLeastOneTwo : List Char -> Bool
atLeastOneTwo digits =
    List.Extra.group digits
        |> List.map (nonemptyListToList >> List.length)
        |> List.filter (\count -> count == 2)
        |> List.length
        |> (/=) 0


monotonic : List Char -> Bool
monotonic digits =
    List.Extra.groupWhile (<=) digits
        |> List.length
        |> (==) 1


nonemptyListToList : ( a, List a ) -> List a
nonemptyListToList ( x, xs ) =
    x :: xs


compute1 : Input1 -> Output1
compute1 ( low, high ) =
    Seq.iterate ((+) 1) low
        |> Seq.take (high - low + 1)
        |> Seq.filterMap
            (\n ->
                let
                    digits =
                        toChars n
                in
                if twoOrMore digits && monotonic digits then
                    Just n

                else
                    Nothing
            )
        |> Seq.length


compute2 : Input2 -> Output2
compute2 ( low, high ) =
    Seq.iterate ((+) 1) low
        |> Seq.take (high - low + 1)
        |> Seq.filterMap
            (\n ->
                let
                    digits =
                        toChars n
                in
                if atLeastOneTwo digits && monotonic digits then
                    Just n

                else
                    Nothing
            )
        |> Seq.length



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
206938-679128
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
