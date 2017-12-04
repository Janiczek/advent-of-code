module Year2017.Day0X exposing (..)

import Advent exposing (Test)


main : Program Never ( Output, Output ) Never
main =
    Advent.program
        { input = input
        , parse1 = parse
        , parse2 = parse
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }


type alias Input =
    Int


type alias Output =
    Int


input : String
input =
    "input"


parse : String -> Input
parse input =
    "parsed " ++ input ++ " 2"


compute1 : Input -> Output
compute1 input =
    "output 1"


compute2 : Input -> Output
compute2 input =
    "output 2"


tests1 : List (Test Input Output)
tests1 =
    [ Test "example"
        "input"
        "parsed input 1"
        "output 1"
    ]


tests2 : List (Test Input Output)
tests2 =
    [ Test "example"
        "input"
        "parsed input 2"
        "output 2"
    ]
