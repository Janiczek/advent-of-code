module Year2017.Day10 exposing (..)

import Advent exposing (Test)
import Char
import Bitwise
import List.Extra
import Hex


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


type alias Input =
    List Int


type alias Output1 =
    Int


type alias Output2 =
    String


parse1 : String -> Input
parse1 input =
    input
        |> String.split ","
        |> List.map Advent.toInt


parse2 : String -> Input
parse2 input =
    input
        |> String.toList
        |> List.map Char.toCode


compute1 : Input -> Output1
compute1 input =
    let
        numbers =
            List.range 0 255

        firstPosition =
            0

        skipSize =
            0
    in
        round input ( numbers, firstPosition, skipSize )
            |> (\( numbers, position, skipSize ) -> numbers)
            |> List.take 2
            |> List.product


round : List Int -> ( List Int, Int, Int ) -> ( List Int, Int, Int )
round lengths ( numbers, position, skipSize ) =
    lengths
        |> List.foldl process
            ( numbers
            , position
            , skipSize
            )


process : Int -> ( List Int, Int, Int ) -> ( List Int, Int, Int )
process length ( numbers, position, skipSize ) =
    let
        isWrapping =
            length + position > listLength

        listLength =
            numbers
                |> List.length

        endLength =
            listLength - position

        startLength =
            length - endLength

        selectedNumbers =
            if isWrapping then
                let
                    start =
                        numbers
                            |> List.take startLength

                    end =
                        numbers
                            |> List.drop position
                in
                    end ++ start
            else
                numbers
                    |> List.drop position
                    |> List.take length

        reversed =
            selectedNumbers
                |> List.reverse

        newNumbers =
            if isWrapping then
                let
                    atEnd =
                        reversed
                            |> List.take endLength

                    atStart =
                        reversed
                            |> List.drop endLength

                    numbersWithout =
                        numbers
                            |> List.drop startLength
                            |> List.take (listLength - endLength - startLength)
                in
                    atStart ++ numbersWithout ++ atEnd
            else
                let
                    numbersBefore =
                        numbers
                            |> List.take position

                    numbersAfter =
                        numbers
                            |> List.drop (position + length)
                in
                    numbersBefore ++ reversed ++ numbersAfter

        newPosition =
            (position + length + skipSize) % listLength

        newSkipSize =
            skipSize + 1
    in
        ( newNumbers, newPosition, newSkipSize )


compute2 : Input -> Output2
compute2 input =
    let
        staticLengths =
            [ 17, 31, 73, 47, 23 ]

        lengths =
            input ++ staticLengths

        numbers =
            List.range 0 255

        firstPosition =
            0

        skipSize =
            0

        rounds =
            64

        sparseHash =
            lengths
                |> List.repeat rounds
                |> List.foldl round ( numbers, firstPosition, skipSize )
                |> (\( numbers, position, skipSize ) -> numbers)

        denseHash =
            sparseHash
                |> List.Extra.groupsOf 16
                |> List.map (List.Extra.foldl1 Bitwise.xor)
                |> List.filterMap identity

        hexed =
            denseHash
                |> List.map paddedHex
                |> String.join ""
    in
        hexed


paddedHex : Int -> String
paddedHex num =
    Hex.toString num
        |> String.padLeft 2 '0'


tests1 : List (Test Input Output1)
tests1 =
    []


tests2 : List (Test Input Output2)
tests2 =
    []


hash : String -> String
hash input =
    input
        |> String.toList
        |> List.map Char.toCode
        |> compute2


input : String
input =
    "83,0,193,1,254,237,187,40,88,27,2,255,149,29,42,100"
