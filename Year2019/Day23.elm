module Year2019.Day23 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Array exposing (Array)
import Array.Extra
import Dict exposing (Dict)
import Dict.Extra
import Fifo exposing (Fifo)
import List.Extra
import Set exposing (Set)
import Year2019.Intcode as Intcode
    exposing
        ( Computer
        , OutputError(..)
        , Stop(..)
        )
import Year2019.Intcode.Memory as Memory exposing (Memory)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    Memory


type alias Input2 =
    Memory


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    Memory.fromString string
        |> Advent.unsafeMaybe "parse1"


parse2 : String -> Input2
parse2 string =
    Memory.fromString string
        |> Advent.unsafeMaybe "parse2"



-- 3. COMPUTE (actually solve the problem)


type alias State1 =
    { computers : Array (Result Stop Computer)
    , queues : Array (Fifo ( Int, Int ))
    }


type alias State2 =
    { computers : Array (Result Stop Computer)
    , queues : Array (Fifo ( Int, Int ))
    , natLast : ( Int, Int )
    , sentLast : Int
    , waitingCount : Int
    }


initComputer : Memory -> Int -> Result Stop Computer
initComputer memory index =
    Intcode.initWithMemory memory
        |> Intcode.addInput index
        |> Intcode.stepUntilStopped


compute1 : Input1 -> Output1
compute1 memory =
    { computers = Array.initialize 50 (initComputer memory)
    , queues = Array.repeat 50 Fifo.empty
    }
        |> go1 0


go1 : Int -> State1 -> Int
go1 index state =
    if index == 50 then
        go1 0 state

    else
        let
            ( maybeInput, newQueue ) =
                state.queues
                    |> Array.get index
                    |> Advent.unsafeMaybe "go1 queue"
                    |> Fifo.remove

            computer =
                state.computers
                    |> Array.get index
                    |> Advent.unsafeMaybe "go1 computer"

            ( outputs, newComputer ) =
                computer
                    |> (case maybeInput of
                            Nothing ->
                                Intcode.addInput_ -1

                            Just ( x, y ) ->
                                Intcode.addInput_ x >> Intcode.addInput_ y
                       )
                    |> Intcode.stepUntilStopped_
                    |> Intcode.getOutputs

            packets =
                outputs
                    |> List.Extra.greedyGroupsOf 3
                    |> List.map
                        (\list ->
                            case list of
                                [ address, x, y ] ->
                                    ( address, x, y )

                                _ ->
                                    Debug.todo "packets wat"
                        )
        in
        case List.filter (\( address, _, _ ) -> address == 255) packets of
            ( _, _, y ) :: _ ->
                y

            [] ->
                let
                    newState =
                        { state
                            | queues =
                                state.queues
                                    |> Array.set index newQueue
                                    |> sendPackets packets
                            , computers = Array.set index newComputer state.computers
                        }
                in
                go1 (index + 1) newState


sendPackets : List ( Int, Int, Int ) -> Array (Fifo ( Int, Int )) -> Array (Fifo ( Int, Int ))
sendPackets packets queues =
    packets
        |> List.foldl (\( address, x, y ) queues_ -> send address ( x, y ) queues_)
            queues


send : Int -> ( Int, Int ) -> Array (Fifo ( Int, Int )) -> Array (Fifo ( Int, Int ))
send address packet queues =
    Array.Extra.update address (Fifo.insert packet) queues


compute2 : Input2 -> Output2
compute2 memory =
    { computers = Array.initialize 50 (initComputer memory)
    , queues = Array.repeat 50 Fifo.empty
    , natLast = ( -1, -1 )
    , sentLast = -1
    , waitingCount = 0
    }
        |> go2 0


go2 : Int -> State2 -> Int
go2 index state =
    if state.waitingCount == 50 then
        if Tuple.second state.natLast == state.sentLast then
            state.sentLast

        else
            go2 0
                { state
                    | waitingCount = 0
                    , queues = send 0 state.natLast state.queues
                    , sentLast = Tuple.second state.natLast
                }

    else if index == 50 then
        go2 0 state

    else
        let
            ( maybeInput, newQueue ) =
                state.queues
                    |> Array.get index
                    |> Advent.unsafeMaybe "go2 queue"
                    |> Fifo.remove

            computer =
                state.computers
                    |> Array.get index
                    |> Advent.unsafeMaybe "go2 computer"

            ( outputs, newComputer ) =
                computer
                    |> (case maybeInput of
                            Nothing ->
                                Intcode.addInput_ -1

                            Just ( x, y ) ->
                                Intcode.addInput_ x >> Intcode.addInput_ y
                       )
                    |> Intcode.stepUntilStopped_
                    |> Intcode.getOutputs

            packets =
                outputs
                    |> List.Extra.greedyGroupsOf 3
                    |> List.map
                        (\list ->
                            case list of
                                [ address, x, y ] ->
                                    ( address, x, y )

                                _ ->
                                    Debug.todo "packets wat"
                        )

            stateWithWaiting =
                { state
                    | waitingCount =
                        if maybeInput == Nothing then
                            state.waitingCount + 1

                        else
                            0
                }
        in
        case List.partition (\( address, _, _ ) -> address == 255) packets of
            ( natPackets, normalPackets ) ->
                go2 (index + 1)
                    { stateWithWaiting
                        | natLast =
                            if List.isEmpty natPackets then
                                stateWithWaiting.natLast

                            else
                                natPackets
                                    |> List.reverse
                                    |> List.head
                                    |> Advent.unsafeMaybe "last packet"
                                    |> (\( _, x, y ) -> ( x, y ))
                        , queues =
                            state.queues
                                |> Array.set index newQueue
                                |> (if List.isEmpty normalPackets then
                                        identity

                                    else
                                        sendPackets normalPackets
                                   )
                        , computers = Array.set index newComputer state.computers
                    }



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example"
        "input"
        Nothing -- Just "parsed-input"
        -1
     -}
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
3,62,1001,62,11,10,109,2231,105,1,0,2005,1087,2085,1966,703,1318,1118,897,860,825,996,1547,1518,1485,2046,1448,1617,1186,1155,1376,668,1413,1935,2126,934,965,635,1252,1058,571,1757,1588,1347,602,1848,1652,2194,1877,1027,734,1906,763,794,1788,1819,1221,1285,1693,2157,1726,0,0,0,0,0,0,0,0,0,0,0,0,3,64,1008,64,-1,62,1006,62,88,1006,61,170,1106,0,73,3,65,20102,1,64,1,20101,0,66,2,21101,0,105,0,1105,1,436,1201,1,-1,64,1007,64,0,62,1005,62,73,7,64,67,62,1006,62,73,1002,64,2,133,1,133,68,133,102,1,0,62,1001,133,1,140,8,0,65,63,2,63,62,62,1005,62,73,1002,64,2,161,1,161,68,161,1101,0,1,0,1001,161,1,169,1001,65,0,0,1102,1,1,61,1101,0,0,63,7,63,67,62,1006,62,203,1002,63,2,194,1,68,194,194,1006,0,73,1001,63,1,63,1105,1,178,21101,0,210,0,105,1,69,2101,0,1,70,1102,0,1,63,7,63,71,62,1006,62,250,1002,63,2,234,1,72,234,234,4,0,101,1,234,240,4,0,4,70,1001,63,1,63,1106,0,218,1105,1,73,109,4,21101,0,0,-3,21102,1,0,-2,20207,-2,67,-1,1206,-1,293,1202,-2,2,283,101,1,283,283,1,68,283,283,22001,0,-3,-3,21201,-2,1,-2,1105,1,263,22101,0,-3,-3,109,-4,2106,0,0,109,4,21102,1,1,-3,21102,1,0,-2,20207,-2,67,-1,1206,-1,342,1202,-2,2,332,101,1,332,332,1,68,332,332,22002,0,-3,-3,21201,-2,1,-2,1106,0,312,21202,-3,1,-3,109,-4,2106,0,0,109,1,101,1,68,358,21001,0,0,1,101,3,68,366,21002,0,1,2,21101,0,376,0,1106,0,436,21202,1,1,0,109,-1,2105,1,0,1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288,1048576,2097152,4194304,8388608,16777216,33554432,67108864,134217728,268435456,536870912,1073741824,2147483648,4294967296,8589934592,17179869184,34359738368,68719476736,137438953472,274877906944,549755813888,1099511627776,2199023255552,4398046511104,8796093022208,17592186044416,35184372088832,70368744177664,140737488355328,281474976710656,562949953421312,1125899906842624,109,8,21202,-6,10,-5,22207,-7,-5,-5,1205,-5,521,21102,1,0,-4,21102,1,0,-3,21102,51,1,-2,21201,-2,-1,-2,1201,-2,385,471,20101,0,0,-1,21202,-3,2,-3,22207,-7,-1,-5,1205,-5,496,21201,-3,1,-3,22102,-1,-1,-5,22201,-7,-5,-7,22207,-3,-6,-5,1205,-5,515,22102,-1,-6,-5,22201,-3,-5,-3,22201,-1,-4,-4,1205,-2,461,1106,0,547,21102,-1,1,-4,21202,-6,-1,-6,21207,-7,0,-5,1205,-5,547,22201,-7,-6,-7,21201,-4,1,-4,1106,0,529,22102,1,-4,-7,109,-8,2105,1,0,109,1,101,1,68,564,20101,0,0,0,109,-1,2105,1,0,1102,45263,1,66,1102,1,1,67,1102,1,598,68,1102,556,1,69,1102,1,1,71,1101,0,600,72,1105,1,73,1,160,35,292542,1102,14557,1,66,1102,1,1,67,1101,0,629,68,1102,1,556,69,1102,1,2,71,1102,1,631,72,1105,1,73,1,11,3,59197,36,43787,1102,353,1,66,1101,1,0,67,1101,662,0,68,1101,0,556,69,1101,2,0,71,1102,664,1,72,1106,0,73,1,6679,11,6326,48,94793,1102,23057,1,66,1102,1,3,67,1102,1,695,68,1101,0,302,69,1101,0,1,71,1102,1,701,72,1105,1,73,0,0,0,0,0,0,6,93059,1102,55381,1,66,1101,0,1,67,1101,0,730,68,1102,1,556,69,1101,0,1,71,1101,732,0,72,1106,0,73,1,1103,11,15815,1102,1,13669,66,1102,1,1,67,1102,761,1,68,1102,1,556,69,1102,1,0,71,1102,763,1,72,1105,1,73,1,1598,1102,1,67489,66,1102,1,1,67,1102,790,1,68,1101,556,0,69,1101,0,1,71,1101,792,0,72,1106,0,73,1,2953,9,200877,1101,79979,0,66,1102,1,1,67,1102,1,821,68,1102,556,1,69,1102,1,1,71,1101,0,823,72,1105,1,73,1,37,36,175148,1101,66959,0,66,1102,3,1,67,1101,852,0,68,1101,0,302,69,1101,0,1,71,1102,858,1,72,1106,0,73,0,0,0,0,0,0,15,72661,1101,0,59359,66,1102,1,4,67,1101,0,887,68,1102,1,302,69,1102,1,1,71,1101,0,895,72,1106,0,73,0,0,0,0,0,0,0,0,15,145322,1101,0,37699,66,1102,1,4,67,1102,1,924,68,1102,1,302,69,1101,0,1,71,1102,932,1,72,1105,1,73,0,0,0,0,0,0,0,0,15,217983,1102,51679,1,66,1102,1,1,67,1102,961,1,68,1102,556,1,69,1102,1,1,71,1102,963,1,72,1106,0,73,1,467,11,3163,1102,50891,1,66,1102,1,1,67,1101,0,992,68,1101,556,0,69,1102,1,1,71,1101,994,0,72,1106,0,73,1,-263,3,295985,1101,67867,0,66,1102,1,1,67,1101,0,1023,68,1102,556,1,69,1101,1,0,71,1101,1025,0,72,1105,1,73,1,-3,48,379172,1101,44267,0,66,1102,1,1,67,1101,1054,0,68,1101,0,556,69,1101,0,1,71,1101,1056,0,72,1106,0,73,1,401,7,37699,1102,1,86069,66,1101,1,0,67,1102,1085,1,68,1102,1,556,69,1102,0,1,71,1102,1,1087,72,1105,1,73,1,1618,1101,19961,0,66,1101,0,1,67,1101,0,1114,68,1101,556,0,69,1102,1,1,71,1102,1116,1,72,1105,1,73,1,173,8,237436,1101,0,93059,66,1102,4,1,67,1102,1145,1,68,1101,253,0,69,1101,0,1,71,1101,0,1153,72,1105,1,73,0,0,0,0,0,0,0,0,27,26573,1101,5351,0,66,1101,0,1,67,1101,1182,0,68,1102,1,556,69,1102,1,1,71,1101,1184,0,72,1105,1,73,1,125,19,56359,1101,17351,0,66,1102,1,3,67,1102,1,1213,68,1101,0,302,69,1102,1,1,71,1101,0,1219,72,1105,1,73,0,0,0,0,0,0,6,279177,1102,52189,1,66,1101,1,0,67,1102,1248,1,68,1101,0,556,69,1101,0,1,71,1102,1,1250,72,1105,1,73,1,26,9,66959,1102,1,26573,66,1101,2,0,67,1101,0,1279,68,1102,351,1,69,1101,1,0,71,1101,0,1283,72,1106,0,73,0,0,0,0,255,22171,1101,0,2467,66,1102,1,2,67,1101,0,1312,68,1102,1,302,69,1102,1,1,71,1102,1316,1,72,1105,1,73,0,0,0,0,6,186118,1101,73091,0,66,1102,1,1,67,1102,1,1345,68,1101,0,556,69,1102,0,1,71,1102,1,1347,72,1106,0,73,1,1703,1102,1,52769,66,1101,1,0,67,1101,1374,0,68,1101,0,556,69,1101,0,0,71,1102,1376,1,72,1105,1,73,1,1080,1102,1,56359,66,1102,4,1,67,1102,1,1403,68,1101,0,302,69,1101,1,0,71,1101,1411,0,72,1106,0,73,0,0,0,0,0,0,0,0,35,195028,1102,1,7573,66,1101,1,0,67,1102,1440,1,68,1101,556,0,69,1101,0,3,71,1101,0,1442,72,1106,0,73,1,3,8,59359,3,177591,7,150796,1102,1,72661,66,1101,0,4,67,1102,1475,1,68,1101,0,253,69,1101,1,0,71,1101,1483,0,72,1106,0,73,0,0,0,0,0,0,0,0,47,1754,1102,1,17981,66,1102,1,1,67,1102,1,1512,68,1101,556,0,69,1102,2,1,71,1101,1514,0,72,1106,0,73,1,10,19,225436,35,243785,1102,44159,1,66,1101,1,0,67,1101,1545,0,68,1101,0,556,69,1102,1,0,71,1102,1547,1,72,1106,0,73,1,1082,1101,0,3163,66,1102,6,1,67,1101,1574,0,68,1101,302,0,69,1102,1,1,71,1102,1,1586,72,1106,0,73,0,0,0,0,0,0,0,0,0,0,0,0,6,372236,1101,0,80681,66,1101,0,1,67,1102,1615,1,68,1102,1,556,69,1101,0,0,71,1102,1,1617,72,1105,1,73,1,1428,1102,1,74377,66,1101,0,1,67,1102,1644,1,68,1102,1,556,69,1102,3,1,71,1102,1,1646,72,1105,1,73,1,5,19,112718,19,169077,35,97514,1101,48757,0,66,1102,1,6,67,1101,1679,0,68,1101,0,302,69,1102,1,1,71,1102,1,1691,72,1105,1,73,0,0,0,0,0,0,0,0,0,0,0,0,27,53146,1102,1,877,66,1102,1,2,67,1101,1720,0,68,1102,302,1,69,1102,1,1,71,1101,1724,0,72,1105,1,73,0,0,0,0,36,131361,1101,79493,0,66,1102,1,1,67,1102,1,1753,68,1102,556,1,69,1101,1,0,71,1102,1,1755,72,1105,1,73,1,23,3,118394,1102,77863,1,66,1102,1,1,67,1102,1784,1,68,1101,556,0,69,1102,1,1,71,1101,1786,0,72,1105,1,73,1,301,8,118718,1102,1,4799,66,1101,0,1,67,1101,1815,0,68,1101,556,0,69,1101,0,1,71,1101,1817,0,72,1105,1,73,1,59,11,18978,1102,1,46889,66,1101,0,1,67,1102,1,1846,68,1102,556,1,69,1102,1,0,71,1101,1848,0,72,1106,0,73,1,1232,1102,7297,1,66,1101,0,1,67,1101,1875,0,68,1102,556,1,69,1101,0,0,71,1102,1,1877,72,1105,1,73,1,1922,1101,0,12893,66,1101,0,1,67,1102,1904,1,68,1101,0,556,69,1102,0,1,71,1101,1906,0,72,1105,1,73,1,1378,1101,78583,0,66,1102,1,1,67,1102,1,1933,68,1101,0,556,69,1102,0,1,71,1102,1,1935,72,1105,1,73,1,1079,1102,101161,1,66,1101,1,0,67,1101,1962,0,68,1101,0,556,69,1101,1,0,71,1101,0,1964,72,1106,0,73,1,4,11,12652,1101,0,59197,66,1101,0,5,67,1101,1993,0,68,1101,0,302,69,1102,1,1,71,1102,2003,1,72,1105,1,73,0,0,0,0,0,0,0,0,0,0,15,290644,1102,1,22171,66,1101,1,0,67,1102,2032,1,68,1101,556,0,69,1102,1,6,71,1102,2034,1,72,1106,0,73,1,22651,46,4934,17,17351,17,34702,20,23057,20,46114,20,69171,1102,1,29669,66,1101,0,1,67,1101,2073,0,68,1101,556,0,69,1101,5,0,71,1101,2075,0,72,1105,1,73,1,1,8,178077,9,133918,3,236788,7,113097,48,284379,1102,1,41863,66,1101,0,1,67,1102,1,2112,68,1102,1,556,69,1102,1,6,71,1102,1,2114,72,1106,0,73,1,2,11,9489,47,877,36,87574,48,189586,35,48757,35,146271,1101,0,65761,66,1102,1,1,67,1101,2153,0,68,1102,1,556,69,1101,1,0,71,1102,1,2155,72,1105,1,73,1,603,7,75398,1102,94793,1,66,1102,1,4,67,1101,2184,0,68,1102,1,302,69,1102,1,1,71,1102,2192,1,72,1105,1,73,0,0,0,0,0,0,0,0,17,52053,1101,0,43787,66,1102,1,4,67,1101,0,2221,68,1102,302,1,69,1102,1,1,71,1102,2229,1,72,1106,0,73,0,0,0,0,0,0,0,0,46,2467
"""
        |> Advent.removeNewlinesAtEnds


main : Program () ( Output1, Output2 ) Never
main =
    Advent.program
        { input = input_
        , parse1 = parse1
        , parse2 = parse2
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }
