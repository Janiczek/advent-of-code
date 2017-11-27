module Year201X.DayXX exposing (..)

import Advent exposing (Test)


main : Program Never Output Never
main =
    Advent.program
        { input = input
        , parse = parse
        , compute = compute
        , tests = tests
        }


type alias Input =
    String


type alias Output =
    String


input : Input
input =
    "input"


parse : String -> Input
parse input =
    "parsed " ++ input


compute : Input -> Output
compute input =
    "output"


tests : List (Test Input Output)
tests =
    [ Test "example"
        "input"
        "parsed input"
        "output"
    ]
