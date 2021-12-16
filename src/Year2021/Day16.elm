module Year2021.Day16 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import BigInt exposing (BigInt)
import List.Extra as List
import RadixInt



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List Int


type alias Input2 =
    List Int


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


fromHex : Char -> List Int
fromHex char =
    case char of
        '0' ->
            [ 0, 0, 0, 0 ]

        '1' ->
            [ 0, 0, 0, 1 ]

        '2' ->
            [ 0, 0, 1, 0 ]

        '3' ->
            [ 0, 0, 1, 1 ]

        '4' ->
            [ 0, 1, 0, 0 ]

        '5' ->
            [ 0, 1, 0, 1 ]

        '6' ->
            [ 0, 1, 1, 0 ]

        '7' ->
            [ 0, 1, 1, 1 ]

        '8' ->
            [ 1, 0, 0, 0 ]

        '9' ->
            [ 1, 0, 0, 1 ]

        'A' ->
            [ 1, 0, 1, 0 ]

        'B' ->
            [ 1, 0, 1, 1 ]

        'C' ->
            [ 1, 1, 0, 0 ]

        'D' ->
            [ 1, 1, 0, 1 ]

        'E' ->
            [ 1, 1, 1, 0 ]

        'F' ->
            [ 1, 1, 1, 1 ]

        _ ->
            Debug.todo "fromHex"


parse1 : String -> Input1
parse1 string =
    string
        |> String.toList
        |> List.concatMap fromHex


parse2 : String -> Input2
parse2 string =
    parse1 string


logBits : String -> List Int -> List Int
logBits label bits =
    let
        _ =
            bits
                |> List.map String.fromInt
                |> String.concat
                |> Debug.log label
    in
    bits



-- 3. COMPUTE (actually solve the problem)


type alias Packet =
    { version : Int
    , data : PacketData
    }


type PacketData
    = LiteralValue BigInt
    | Operator OperatorType (List Packet)


type OperatorType
    = Sum
    | Product
    | Minimum
    | Maximum
    | GreaterThan
    | LessThan
    | EqualTo


toInt : List Int -> Int
toInt list =
    list
        |> List.reverse
        |> RadixInt.fromList (RadixInt.Base 2)
        |> Maybe.map RadixInt.toInt
        |> Maybe.withDefault -1


toPacketData : Int -> List Int -> ( PacketData, List Int )
toPacketData typeId bits =
    let
        operator : OperatorType -> ( PacketData, List Int )
        operator opType =
            case bits of
                i :: lengthAndPackets ->
                    if i == 0 then
                        -- next 15 bits are a number representing the bit length of the subpackets
                        let
                            ( before, after ) =
                                List.splitAt 15 lengthAndPackets

                            bitLength =
                                toInt before

                            ( usableBits, rest ) =
                                List.splitAt bitLength after

                            subpackets =
                                ( [], usableBits )
                                    |> Advent.doUntil
                                        (\_ ( _, usableBits_ ) -> List.isEmpty usableBits_)
                                        (\( acc, accRest ) ->
                                            let
                                                ( newPacket, newRest ) =
                                                    parsePacket accRest
                                            in
                                            ( newPacket :: acc, newRest )
                                        )
                                    |> Tuple.first
                                    |> List.reverse
                        in
                        ( Operator opType subpackets, rest )

                    else
                        -- next 11 bits are a number representing the subpackets count
                        let
                            ( before, after ) =
                                List.splitAt 11 lengthAndPackets

                            subpacketsCount =
                                toInt before

                            ( subpackets, rest ) =
                                ( [], after )
                                    |> Advent.doNTimes subpacketsCount
                                        (\( acc, accRest ) ->
                                            let
                                                ( newPacket, newRest ) =
                                                    parsePacket accRest
                                            in
                                            ( newPacket :: acc, newRest )
                                        )
                                    |> Tuple.mapFirst List.reverse
                        in
                        ( Operator opType subpackets, rest )

                [] ->
                    Debug.todo "toPacketData didn't get the I bit"
    in
    case typeId of
        0 ->
            operator Sum

        1 ->
            operator Product

        2 ->
            operator Minimum

        3 ->
            operator Maximum

        5 ->
            operator GreaterThan

        6 ->
            operator LessThan

        7 ->
            operator EqualTo

        4 ->
            let
                go : BigInt -> List Int -> ( BigInt, List Int )
                go nSoFar accBits =
                    case accBits of
                        continueIndicator :: b0 :: b1 :: b2 :: b3 :: rest ->
                            let
                                currentVal : BigInt
                                currentVal =
                                    [ b0, b1, b2, b3 ]
                                        |> toInt
                                        |> BigInt.fromInt

                                newN : BigInt
                                newN =
                                    BigInt.add
                                        (BigInt.mul (BigInt.fromInt 16) nSoFar)
                                        currentVal
                            in
                            if continueIndicator == 1 then
                                -- we'll continue
                                go newN rest

                            else
                                -- we're ending!
                                ( newN, rest )

                        _ ->
                            Debug.todo "weird literal value bits"

                ( value, unusedBits ) =
                    go (BigInt.fromInt 0) bits
            in
            ( LiteralValue value
            , unusedBits
            )

        _ ->
            Debug.todo "weird type ID"


parsePacket : List Int -> ( Packet, List Int )
parsePacket ints =
    case ints of
        v1 :: v2 :: v3 :: t1 :: t2 :: t3 :: rest ->
            let
                ( data, unusedBits ) =
                    toPacketData (toInt [ t1, t2, t3 ]) rest
            in
            ( { version = toInt [ v1, v2, v3 ]
              , data = data
              }
            , unusedBits
            )

        _ ->
            Debug.todo "parse packet"


sumVersionNumbers : Packet -> Int
sumVersionNumbers packet =
    packet.version
        + (case packet.data of
            LiteralValue _ ->
                0

            Operator _ packets ->
                List.sum <| List.map sumVersionNumbers packets
          )


eval : Packet -> Int
eval packet =
    case packet.data of
        LiteralValue n ->
            n
                |> BigInt.toString
                |> String.toInt
                |> Advent.unsafeMaybe "eval literal"

        Operator opType packets ->
            let
                boolFn op list =
                    case list of
                        [ a, b ] ->
                            if op a b then
                                1

                            else
                                0

                        _ ->
                            Debug.todo "op didn't have 2 args"

                fn =
                    case opType of
                        Sum ->
                            List.sum

                        Product ->
                            List.product

                        Minimum ->
                            List.minimum
                                >> Advent.unsafeMaybe "minimum"

                        Maximum ->
                            List.maximum
                                >> Advent.unsafeMaybe "maximum"

                        GreaterThan ->
                            boolFn (>)

                        LessThan ->
                            boolFn (<)

                        EqualTo ->
                            boolFn (==)
            in
            fn (List.map eval packets)


compute1 : Input1 -> Output1
compute1 input =
    input
        |> parsePacket
        |> Tuple.first
        |> sumVersionNumbers


compute2 : Input2 -> Output2
compute2 input =
    input
        |> parsePacket
        |> Tuple.first
        |> eval



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "example 1" "8A004A801A8002F478" Nothing 16
    , Test "example 2" "620080001611562C8802118E34" Nothing 12
    , Test "example 3" "C0015000016115A2E0802F182340" Nothing 23
    , Test "example 4" "A0016C880162017C3686B18A3D4780" Nothing 31
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    [ Test "example 1" "C200B40A82" Nothing 3
    , Test "example 2" "04005AC33890" Nothing 54
    , Test "example 3" "880086C3E88112" Nothing 7
    , Test "example 4" "CE00C43D881120" Nothing 9
    , Test "example 5" "D8005AC2A8F0" Nothing 1
    , Test "example 6" "F600BC2D8F" Nothing 0
    , Test "example 7" "9C005AC2F8F0" Nothing 0
    , Test "example 8" "9C0141080250320F1802104A08" Nothing 1
    ]



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
20546718027401204FE775D747A5AD3C3CCEEB24CC01CA4DFF2593378D645708A56D5BD704CC0110C469BEF2A4929689D1006AF600AC942B0BA0C942B0BA24F9DA8023377E5AC7535084BC6A4020D4C73DB78F005A52BBEEA441255B42995A300AA59C27086618A686E71240005A8C73D4CF0AC40169C739584BE2E40157D0025533770940695FE982486C802DD9DC56F9F07580291C64AAAC402435802E00087C1E8250440010A8C705A3ACA112001AF251B2C9009A92D8EBA6006A0200F4228F50E80010D8A7052280003AD31D658A9231AA34E50FC8010694089F41000C6A73F4EDFB6C9CC3E97AF5C61A10095FE00B80021B13E3D41600042E13C6E8912D4176002BE6B060001F74AE72C7314CEAD3AB14D184DE62EB03880208893C008042C91D8F9801726CEE00BCBDDEE3F18045348F34293E09329B24568014DCADB2DD33AEF66273DA45300567ED827A00B8657B2E42FD3795ECB90BF4C1C0289D0695A6B07F30B93ACB35FBFA6C2A007A01898005CD2801A60058013968048EB010D6803DE000E1C6006B00B9CC028D8008DC401DD9006146005980168009E1801B37E02200C9B0012A998BACB2EC8E3D0FC8262C1009D00008644F8510F0401B825182380803506A12421200CB677011E00AC8C6DA2E918DB454401976802F29AA324A6A8C12B3FD978004EB30076194278BE600C44289B05C8010B8FF1A6239802F3F0FFF7511D0056364B4B18B034BDFB7173004740111007230C5A8B6000874498E30A27BF92B3007A786A51027D7540209A04821279D41AA6B54C15CBB4CC3648E8325B490401CD4DAFE004D932792708F3D4F769E28500BE5AF4949766DC24BB5A2C4DC3FC3B9486A7A0D2008EA7B659A00B4B8ACA8D90056FA00ACBCAA272F2A8A4FB51802929D46A00D58401F8631863700021513219C11200996C01099FBBCE6285106
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
