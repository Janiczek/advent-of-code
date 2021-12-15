module Year2017.Day13 exposing (..)

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


type alias Depth =
    Int


type alias Range =
    Int


type alias Severity =
    Int


type alias ScannerPosition =
    Int


type alias MovingUp =
    Bool


type alias Layer =
    ( Depth, Range, ScannerPosition, MovingUp, Severity )


type alias Input =
    List Layer


type alias Output =
    Severity


parse : String -> Input
parse input =
    input
        |> String.lines
        |> List.map parseLayer


parseLayer : String -> Layer
parseLayer string =
    case String.split ": " string of
        [ a, b ] ->
            ( Advent.toInt a
            , Advent.toInt b
            , 0
            , False
            , 0
            )

        _ ->
            Debug.crash "wrong input!"


compute1 : Input -> Output
compute1 input =
    List.range 0 (maxDepth input)
        |> List.foldl moveScannersAndApplySeverity input
        |> List.map (\( _, _, _, _, s ) -> s)
        |> List.sum


maxDepth : Input -> Int
maxDepth layers =
    layers
        |> List.map (\( d, _, _, _, _ ) -> d)
        |> List.maximum
        |> unsafeMaybe


severityOnCurrentLayer :
    Depth
    -> List Layer
    -> Int
severityOnCurrentLayer depth layers =
    let
        ( _, range, position, isMovingUp, severity ) =
            currentScanner depth layers

        isFound =
            position == 0

        currentSeverity =
            if isFound then
                depth * range
            else
                0
    in
        currentSeverity


moveScannersAndApplySeverity :
    Depth
    -> List Layer
    -> List Layer
moveScannersAndApplySeverity depth layers =
    layers
        --|> Debug.log ("move scanners " ++ toString depth)
        |> List.map
            (\( d, r, p, m, s ) ->
                let
                    newMovingUp =
                        if m then
                            p /= 0
                        else
                            -- moving down
                            p == (r - 1)

                    newPosition =
                        if newMovingUp then
                            p - 1
                        else
                            p + 1

                    newSeverity =
                        if depth == d then
                            severityOnCurrentLayer depth layers
                        else
                            s
                in
                    ( d, r, newPosition, newMovingUp, newSeverity )
            )


currentScanner :
    Depth
    -> List Layer
    -> Layer
currentScanner depth layers =
    layers
        |> List.filter (\( d, _, _, _, _ ) -> d == depth)
        |> List.head
        |> unsafeMaybe


unsafeMaybe : Maybe a -> a
unsafeMaybe maybe =
    case maybe of
        Just x ->
            x

        Nothing ->
            Debug.crash "unsafeMaybe"


compute2 : Input -> Output
compute2 input =
    tryDelay 0 input


tryDelay : Int -> List Layer -> Int
tryDelay delay layers =
    let
        period range =
            -- second -1 because of the off-by-one caused by the order of events
            (range * 2 - 1) - 1

        currentScannerPosition delay depth =
            delay + depth

        allPositionsNotZero =
            layers
                |> List.all
                    (\( depth, range, _, _, _ ) ->
                        let
                            position =
                                currentScannerPosition delay depth

                            per =
                                period range

                            modulo =
                                position % per
                        in
                            modulo /= 0
                    )
    in
        if allPositionsNotZero then
            delay
        else
            tryDelay (delay + 1) layers


tests1 : List (Test Input Output)
tests1 =
    [ Test "example"
        """0: 3
1: 2
4: 4
6: 4"""
        [ ( 0, 3, 0, False, 0 ), ( 1, 2, 0, False, 0 ), ( 4, 4, 0, False, 0 ), ( 6, 4, 0, False, 0 ) ]
        24
    ]


tests2 : List (Test Input Output)
tests2 =
    [ Test "example"
        """0: 3
1: 2
4: 4
6: 4"""
        [ ( 0, 3, 0, False, 0 ), ( 1, 2, 0, False, 0 ), ( 4, 4, 0, False, 0 ), ( 6, 4, 0, False, 0 ) ]
        10
    ]


input : String
input =
    """0: 3
1: 2
2: 4
4: 6
6: 4
8: 6
10: 5
12: 6
14: 9
16: 6
18: 8
20: 8
22: 8
24: 8
26: 8
28: 8
30: 12
32: 14
34: 10
36: 12
38: 12
40: 10
42: 12
44: 12
46: 12
48: 12
50: 12
52: 14
54: 14
56: 12
62: 12
64: 14
66: 14
68: 14
70: 17
72: 14
74: 14
76: 14
82: 14
86: 18
88: 14
96: 14
98: 44"""
