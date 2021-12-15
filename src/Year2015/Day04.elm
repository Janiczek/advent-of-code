module Year2015.Day04 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import MD5



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    String


type alias Input2 =
    String


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    let
        search : Int -> String -> Int
        search suffix string =
            if String.left 5 (MD5.hex (string ++ String.fromInt suffix)) == "00000" then
                suffix

            else
                search (suffix + 1) string
    in
    search 1 input


compute2 : Input2 -> Output2
compute2 input =
    let
        search : Int -> String -> Int
        search suffix string =
            if
                String.left 6
                    (MD5.hex
                        (string
                            ++ String.fromInt
                                (if (suffix |> modBy 100000) == 0 then
                                    Debug.log "i" suffix

                                 else
                                    suffix
                                )
                        )
                    )
                    == "000000"
            then
                suffix

            else
                search (suffix + 1) string
    in
    search 1 input



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example 1"
        "abcdef"
        "abcdef"
        609043
     -}
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
yzbqklnj
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
