module Year2016.Day02 exposing (..)

import Advent exposing (Test, Test)


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
    List (List Direction)


type alias Output1 =
    List Digit


type alias Output2 =
    List Digit2


input : String
input =
    "LURLDDLDULRURDUDLRULRDLLRURDUDRLLRLRURDRULDLRLRRDDULUDULURULLURLURRRLLDURURLLUURDLLDUUDRRDLDLLRUUDURURRULURUURLDLLLUDDUUDRULLRUDURRLRLLDRRUDULLDUUUDLDLRLLRLULDLRLUDLRRULDDDURLUULRDLRULRDURDURUUUDDRRDRRUDULDUUULLLLURRDDUULDRDRLULRRRUUDUURDULDDRLDRDLLDDLRDLDULUDDLULUDRLULRRRRUUUDULULDLUDUUUUDURLUDRDLLDDRULUURDRRRDRLDLLURLULDULRUDRDDUDDLRLRRDUDDRULRULULRDDDDRDLLLRURDDDDRDRUDUDUUDRUDLDULRUULLRRLURRRRUUDRDLDUDDLUDRRURLRDDLUUDUDUUDRLUURURRURDRRRURULUUDUUDURUUURDDDURUDLRLLULRULRDURLLDDULLDULULDDDRUDDDUUDDUDDRRRURRUURRRRURUDRRDLRDUUULLRRRUDD\nDLDUDULDLRDLUDDLLRLUUULLDURRUDLLDUDDRDRLRDDUUUURDULDULLRDRURDLULRUURRDLULUDRURDULLDRURUULLDLLUDRLUDRUDRURURUULRDLLDDDLRUDUDLUDURLDDLRRUUURDDDRLUDDDUDDLDUDDUUUUUULLRDRRUDRUDDDLLLDRDUULRLDURLLDURUDDLLURDDLULLDDDRLUDRDDLDLDLRLURRDURRRUDRRDUUDDRLLUDLDRLRDUDLDLRDRUDUUULULUDRRULUDRDRRLLDDRDDDLULURUURULLRRRRRDDRDDRRRDLRDURURRRDDULLUULRULURURDRRUDURDDUURDUURUURUULURUUDULURRDLRRUUDRLLDLDRRRULDRLLRLDUDULRRLDUDDUUURDUDLDDDUDL\nRURDRUDUUUUULLLUULDULLLDRUULURLDULULRDDLRLLRURULLLLLLRULLURRDLULLUULRRDURRURLUDLULDLRRULRDLDULLDDRRDLLRURRDULULDRRDDULDURRRUUURUDDURULUUDURUULUDLUURRLDLRDDUUUUURULDRDUDDULULRDRUUURRRDRLURRLUUULRUDRRLUDRDLDUDDRDRRUULLLLDUUUULDULRRRLLRLRLRULDLRURRLRLDLRRDRDRLDRUDDDUUDRLLUUURLRLULURLDRRULRULUDRUUURRUDLDDRRDDURUUULLDDLLDDRUDDDUULUDRDDLULDDDDRULDDDDUUUURRLDUURULRDDRDLLLRRDDURUDRRLDUDULRULDDLDDLDUUUULDLLULUUDDULUUDLRDRUDLURDULUDDRDRDRDDURDLURLULRUURDUDULDDLDDRUULLRDRLRRUURRDDRDUDDLRRLLDRDLUUDRRDDDUUUDLRRLDDDUDRURRDDUULUDLLLRUDDRULRLLLRDLUDUUUUURLRRUDUDDDDLRLLULLUDRDURDDULULRDRDLUDDRLURRLRRULRL\nLDUURLLULRUURRDLDRUULRDRDDDRULDLURDDRURULLRUURRLRRLDRURRDRLUDRUUUULLDRLURDRLRUDDRDDDUURRDRRURULLLDRDRDLDUURLDRUULLDRDDRRDRDUUDLURUDDLLUUDDULDDULRDDUUDDDLRLLLULLDLUDRRLDUUDRUUDUDUURULDRRLRRDLRLURDRURURRDURDURRUDLRURURUUDURURUDRURULLLLLUDRUDUDULRLLLRDRLLRLRLRRDULRUUULURLRRLDRRRDRULRUDUURRRRULDDLRULDRRRDLDRLUDLLUDDRURLURURRLRUDLRLLRDLLDRDDLDUDRDLDDRULDDULUDDLLDURDULLDURRURRULLDRLUURURLLUDDRLRRUUDULRRLLRUDRDUURLDDLLURRDLRUURLLDRDLRUULUDURRDULUULDDLUUUDDLRRDRDUDLRUULDDDLDDRUDDD\nDRRDRRURURUDDDRULRUDLDLDULRLDURURUUURURLURURDDDDRULUDLDDRDDUDULRUUULRDUDULURLRULRDDLDUDLDLULRULDRRLUDLLLLURUDUDLLDLDRLRUUULRDDLUURDRRDLUDUDRULRRDDRRLDUDLLDLURLRDLRUUDLDULURDDUUDDLRDLUURLDLRLRDLLRUDRDUURDDLDDLURRDDRDRURULURRLRLDURLRRUUUDDUUDRDRULRDLURLDDDRURUDRULDURUUUUDULURUDDDDUURULULDRURRDRDURUUURURLLDRDLDLRDDULDRLLDUDUDDLRLLRLRUUDLUDDULRLDLLRLUUDLLLUUDULRDULDLRRLDDDDUDDRRRDDRDDUDRLLLDLLDLLRDLDRDLUDRRRLDDRLUDLRLDRUURUDURDLRDDULRLDUUUDRLLDRLDLLDLDRRRLLULLUDDDLRUDULDDDLDRRLLRDDLDUULRDLRRLRLLRUUULLRDUDLRURRRUULLULLLRRURLRDULLLRLDUUUDDRLRLUURRLUUUDURLRDURRDUDDUDDRDDRUD"


type Direction
    = Up
    | Down
    | Left
    | Right


type Digit
    = D1
    | D2
    | D3
    | D4
    | D5
    | D6
    | D7
    | D8
    | D9


type Digit2
    = Dd1
    | Dd2
    | Dd3
    | Dd4
    | Dd5
    | Dd6
    | Dd7
    | Dd8
    | Dd9
    | DdA
    | DdB
    | DdC
    | DdD


parse : String -> Input
parse input =
    input
        |> String.lines
        |> List.map (\row -> row |> String.toList |> List.map parseCharacter)


parseCharacter : Char -> Direction
parseCharacter char =
    case char of
        'U' ->
            Up

        'D' ->
            Down

        'L' ->
            Left

        'R' ->
            Right

        _ ->
            Debug.crash "Wrong input!"


nextDigit : Direction -> Digit -> Digit
nextDigit direction digit =
    case digit of
        D1 ->
            case direction of
                Up ->
                    D1

                Down ->
                    D4

                Left ->
                    D1

                Right ->
                    D2

        D2 ->
            case direction of
                Up ->
                    D2

                Down ->
                    D5

                Left ->
                    D1

                Right ->
                    D3

        D3 ->
            case direction of
                Up ->
                    D3

                Down ->
                    D6

                Left ->
                    D2

                Right ->
                    D3

        D4 ->
            case direction of
                Up ->
                    D1

                Down ->
                    D7

                Left ->
                    D4

                Right ->
                    D5

        D5 ->
            case direction of
                Up ->
                    D2

                Down ->
                    D8

                Left ->
                    D4

                Right ->
                    D6

        D6 ->
            case direction of
                Up ->
                    D3

                Down ->
                    D9

                Left ->
                    D5

                Right ->
                    D6

        D7 ->
            case direction of
                Up ->
                    D4

                Down ->
                    D7

                Left ->
                    D7

                Right ->
                    D8

        D8 ->
            case direction of
                Up ->
                    D5

                Down ->
                    D8

                Left ->
                    D7

                Right ->
                    D9

        D9 ->
            case direction of
                Up ->
                    D6

                Down ->
                    D9

                Left ->
                    D8

                Right ->
                    D9


nextDigit2 : Direction -> Digit2 -> Digit2
nextDigit2 direction digit =
    case digit of
        Dd1 ->
            case direction of
                Up ->
                    Dd1

                Down ->
                    Dd3

                Left ->
                    Dd1

                Right ->
                    Dd1

        Dd2 ->
            case direction of
                Up ->
                    Dd2

                Down ->
                    Dd6

                Left ->
                    Dd2

                Right ->
                    Dd3

        Dd3 ->
            case direction of
                Up ->
                    Dd1

                Down ->
                    Dd7

                Left ->
                    Dd2

                Right ->
                    Dd4

        Dd4 ->
            case direction of
                Up ->
                    Dd4

                Down ->
                    Dd8

                Left ->
                    Dd3

                Right ->
                    Dd4

        Dd5 ->
            case direction of
                Up ->
                    Dd5

                Down ->
                    Dd5

                Left ->
                    Dd5

                Right ->
                    Dd6

        Dd6 ->
            case direction of
                Up ->
                    Dd2

                Down ->
                    DdA

                Left ->
                    Dd5

                Right ->
                    Dd7

        Dd7 ->
            case direction of
                Up ->
                    Dd3

                Down ->
                    DdB

                Left ->
                    Dd6

                Right ->
                    Dd8

        Dd8 ->
            case direction of
                Up ->
                    Dd4

                Down ->
                    DdC

                Left ->
                    Dd7

                Right ->
                    Dd9

        Dd9 ->
            case direction of
                Up ->
                    Dd9

                Down ->
                    Dd9

                Left ->
                    Dd8

                Right ->
                    Dd9

        DdA ->
            case direction of
                Up ->
                    Dd6

                Down ->
                    DdA

                Left ->
                    DdA

                Right ->
                    DdB

        DdB ->
            case direction of
                Up ->
                    Dd7

                Down ->
                    DdD

                Left ->
                    DdA

                Right ->
                    DdC

        DdC ->
            case direction of
                Up ->
                    Dd8

                Down ->
                    DdC

                Left ->
                    DdB

                Right ->
                    DdC

        DdD ->
            case direction of
                Up ->
                    DdB

                Down ->
                    DdD

                Left ->
                    DdD

                Right ->
                    DdD


startingDigit : Digit
startingDigit =
    D5


startingDigit2 : Digit2
startingDigit2 =
    Dd5


compute1 : Input -> Output1
compute1 input =
    input
        |> List.scanl processRow startingDigit
        |> List.tail
        |> Maybe.withDefault []


processRow : List Direction -> Digit -> Digit
processRow directions digit =
    directions
        |> List.foldl nextDigit digit


compute2 : Input -> Output2
compute2 input =
    input
        |> List.scanl processRow2 startingDigit2
        |> List.tail
        |> Maybe.withDefault []


processRow2 : List Direction -> Digit2 -> Digit2
processRow2 directions digit =
    directions
        |> List.foldl nextDigit2 digit


tests1 : List (Test Input Output1)
tests1 =
    [ Test "example"
        "ULL\nRRDDD\nLURDL\nUUUUD"
        [ [ Up, Left, Left ], [ Right, Right, Down, Down, Down ], [ Left, Up, Right, Down, Left ], [ Up, Up, Up, Up, Down ] ]
        [ D1, D9, D8, D5 ]
    ]


tests2 : List (Test Input Output2)
tests2 =
    [ Test "example"
        "ULL\nRRDDD\nLURDL\nUUUUD"
        [ [ Up, Left, Left ], [ Right, Right, Down, Down, Down ], [ Left, Up, Right, Down, Left ], [ Up, Up, Up, Up, Down ] ]
        [ Dd5, DdD, DdB, Dd3 ]
    ]
