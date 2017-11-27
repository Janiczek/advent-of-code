module Year201X.DayXX exposing (..)

import Advent exposing (Test)


main : Program Never ( Output1, Output2 ) Never
main =
    Advent.program
        { input = input
        , parse = parse
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }


type alias Input =
    String


type alias Output1 =
    String


type alias Output2 =
    String


input : String
input =
    "input"


parse : String -> Input
parse input =
    "parsed " ++ input


compute1 : Input -> Output1
compute1 input =
    "output 1"


compute2 : Input -> Output2
compute2 input =
    "output 2"


tests1 : List (Test1 Input Output1)
tests1 =
    [ Test "example"
        "input"
        "parsed input"
        "output 1"
    ]


tests2 : List (Test2 Output2)
tests2 =
    [ Test "example"
        "input"
        "output 2"
    ]
