module Year201X.DayXX exposing (..)

import Advent exposing (Test)


main : Program Never ( Output1, Output2 ) Never
main =
    Advent.program
        { input = input
        , parse1 = parse1
        , parse2 = parse2
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }


type alias Input1 =
    String


type alias Input2 =
    String


type alias Output1 =
    String


type alias Output2 =
    String


input : String
input =
    "input"


parse1 : String -> Input1
parse1 input =
    "parsed " ++ input ++ " 1"


parse2 : String -> Input2
parse2 input =
    "parsed " ++ input ++ " 2"


compute1 : Input -> Output1
compute1 input =
    "output 1"


compute2 : Input -> Output2
compute2 input =
    "output 2"


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test1 "example"
        "input"
        "parsed input 1"
        "output 1"
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    [ Test "example"
        "input"
        "parsed input 2"
        "output 2"
    ]
