module Year201X.DayXX exposing (..)

import Advent


main : Program Never Output Never
main =
    Advent.program
        { input = input
        , init = init
        }


type alias Input =
    String


type alias Output =
    String


input : Input
input =
    "input"


init : Input -> Output
init input =
    "output"
