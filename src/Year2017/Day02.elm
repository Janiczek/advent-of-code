module Year2017.Day02 exposing (..)

import Advent exposing (Test, toInt)
import List.Extra


main : Program Never ( Output1, Output2 ) Never
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
    List (List Int)


type alias Output1 =
    Int


type alias Output2 =
    Int


input : String
input =
    """3751 3769 2769 2039 2794 240 3579 1228 4291 220 324 3960 211 1346 237 1586
550 589 538 110 167 567 99 203 524 288 500 111 118 185 505 74
2127 1904 199 221 1201 250 1119 377 1633 1801 2011 1794 394 238 206 680
435 1703 1385 1461 213 1211 192 1553 1580 197 571 195 326 1491 869 1282
109 104 3033 120 652 2752 1822 2518 1289 1053 1397 951 3015 3016 125 1782
2025 1920 1891 99 1057 1909 2237 106 97 920 603 1841 2150 1980 1970 88
1870 170 167 176 306 1909 1825 1709 168 1400 359 817 1678 1718 1594 1552
98 81 216 677 572 295 38 574 403 74 91 534 662 588 511 51
453 1153 666 695 63 69 68 58 524 1088 75 1117 1192 1232 1046 443
3893 441 1825 3730 3660 115 4503 4105 3495 4092 48 3852 132 156 150 4229
867 44 571 40 884 922 418 328 901 845 42 860 932 53 432 569
905 717 162 4536 4219 179 990 374 4409 4821 393 4181 4054 4958 186 193
2610 2936 218 2552 3281 761 204 3433 3699 2727 3065 3624 193 926 1866 236
2602 216 495 3733 183 4688 2893 4042 3066 3810 189 4392 3900 4321 2814 159
166 136 80 185 135 78 177 123 82 150 121 145 115 63 68 24
214 221 265 766 959 1038 226 1188 1122 117 458 1105 1285 1017 274 281"""


parse : String -> Input
parse input =
    input
        |> String.lines
        |> List.map (String.split " ")
        |> List.map (List.map toInt)


compute1 : Input -> Output1
compute1 input =
    input
        |> List.map differenceOfRow
        |> List.sum


compute2 : Input -> Output2
compute2 input =
    input
        |> List.map resultOfDivision
        |> List.sum


differenceOfRow : List Int -> Int
differenceOfRow row =
    let
        highest =
            List.maximum row

        lowest =
            List.minimum row
    in
        Maybe.map2 (-)
            highest
            lowest
            |> Maybe.withDefault 0


resultOfDivision : List Int -> Int
resultOfDivision row =
    let
        getCombinations : List Int -> List ( Int, Int )
        getCombinations row =
            row
                |> List.Extra.andThen
                    (\x ->
                        row
                            |> List.Extra.andThen (\y -> [ ( x, y ) ])
                    )

        divides : Int -> Int -> Bool
        divides y x =
            x % y == 0
    in
        row
            |> getCombinations
            |> List.filter (\( x, y ) -> x /= y)
            |> List.filter (\( x, y ) -> x |> divides y)
            |> List.map (\( x, y ) -> x // y)
            |> List.sum


tests1 : List (Test Input Output1)
tests1 =
    [ Test "example"
        "5 1 9 5\n7 5 3\n2 4 6 8"
        [ [ 5, 1, 9, 5 ], [ 7, 5, 3 ], [ 2, 4, 6, 8 ] ]
        18
    ]


tests2 : List (Test Input Output2)
tests2 =
    [ Test "example"
        "5 9 2 8\n9 4 7 3\n3 8 6 5"
        [ [ 5, 9, 2, 8 ], [ 9, 4, 7, 3 ], [ 3, 8, 6, 5 ] ]
        9
    ]
