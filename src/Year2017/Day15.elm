module Year2017.Day15 exposing (..)

import Advent exposing (Test)
import Bitwise


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


factorA : Int
factorA =
    16807


factorB : Int
factorB =
    48271


mod : Int
mod =
    2147483647


type alias Input =
    ( Int, Int )


type alias Output =
    Int


parse : String -> Input
parse input =
    case
        input
            |> String.lines
            |> List.map (String.dropLeft 24)
            |> List.map Advent.toInt
    of
        [ a, b ] ->
            ( a, b )

        _ ->
            Debug.crash "wrong input!"


compute1 : Input -> Output
compute1 ( seedA, seedB ) =
    iterate 40000000 0 seedA seedB False


compute2 : Input -> Output
compute2 ( seedA, seedB ) =
    iterate 5000000 0 seedA seedB True


iterate : Int -> Int -> Int -> Int -> Bool -> Int
iterate howManyLeft goodCount currentA currentB findMultiples =
    if howManyLeft == 0 then
        goodCount
    else
        let
            newA =
                if findMultiples then
                    findGood 4 currentA factorA
                else
                    currentA * factorA % mod

            newB =
                if findMultiples then
                    findGood 8 currentB factorB
                else
                    currentB * factorB % mod

            good =
                isGood newA newB

            newGoodCount =
                if good then
                    goodCount + 1
                else
                    goodCount

            newHowManyLeft =
                howManyLeft - 1
        in
            iterate newHowManyLeft newGoodCount newA newB findMultiples


findGood : Int -> Int -> Int -> Int
findGood multiple current factor =
    let
        try =
            current * factor % mod
    in
        if try % multiple == 0 then
            try
        else
            findGood multiple try factor


mask : Int
mask =
    0xFFFF


isGood : Int -> Int -> Bool
isGood a b =
    (Bitwise.and a mask) == (Bitwise.and b mask)


tests1 : List (Test Input Output)
tests1 =
    [ Test "example"
        """Generator A starts with 65
Generator B starts with 8921"""
        ( 65, 8921 )
        588
    ]


tests2 : List (Test Input Output)
tests2 =
    [ Test "example"
        """Generator A starts with 65
Generator B starts with 8921"""
        ( 65, 8921 )
        309
    ]


input : String
input =
    """Generator A starts with 679
Generator B starts with 771"""
