module Year2017.Day01 exposing (..)

import Advent exposing (Test)
import List.Extra


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
    List Int


type alias Output =
    Int


input : String
input =
    "649713959682898259577777982349515784822684939966191359164369933435366431847754488661965363557985166219358714739318371382388296151195361571216131925158492441461844687324923315381358331571577613789649166486152237945917987977793891739865149734755993241361886336926538482271124755359572791451335842534893192693558659991171983849285489139421425933638614884415896938914992732492192458636484523228244532331587584779552788544667253577324649915274115924611758345676183443982992733966373498385685965768929241477983727921279826727976872556315428434799161759734932659829934562339385328119656823483954856427365892627728163524721467938449943358192632262354854593635831559352247443975945144163183563723562891357859367964126289445982135523535923113589316417623483631637569291941782992213889513714525342468563349385271884221685549996534333765731243895662624829924982971685443825366827923589435254514211489649482374876434549682785459698885521673258939413255158196525696236457911447599947449665542554251486847388823576937167237476556782133227279324526834946534444718161524129285919477959937684728882592779941734186144138883994322742484853925383518651687147246943421311287324867663698432546619583638976637733345251834869985746385371617743498627111441933546356934671639545342515392536574744795732243617113574641284231928489312683617154536648219244996491745718658151648246791826466973654765284263928884137863647623237345882469142933142637583644258427416972595241737254449718531724176538648369253796688931245191382956961544775856872281317743828552629843551844927913147518377362266554334386721313244223233396453291224932499277961525785755863852487141946626663835195286762947172384186667439516367219391823774338692151926472717373235612911848773387771244144969149482477519437822863422662157461968444281972353149695515494992537927492111388193837553844671719291482442337761321272333982924289323437277224565149928416255435841327756139118119744528993269157174414264387573331116323982614862952264597611885999285995516357519648695594299657387614793341626318866519144574571816535351149394735916975448425618171572917195165594323552199346814729617189679698944337146"


toInt : String -> Int
toInt string =
    string
        |> String.toInt
        |> Result.mapError (\_ -> Debug.crash "Wrong input!")
        |> Result.withDefault 0


parse : String -> Input
parse input =
    input
        |> String.toList
        |> List.map String.fromChar
        |> List.map toInt


compute1 : Input -> Output
compute1 input =
    input
        |> normalize1
        |> List.Extra.groupsOfWithStep 2 1
        |> List.map
            (\group ->
                case group of
                    [ a, b ] ->
                        if a == b then
                            a
                        else
                            0

                    _ ->
                        Debug.crash "weird?"
            )
        |> List.sum


normalize1 : List Int -> List Int
normalize1 ints =
    case ints of
        x :: xs ->
            ints ++ [ x ]

        _ ->
            ints


normalize2 : List Int -> List Int
normalize2 ints =
    let
        length =
            ints
                |> List.length

        toAdd =
            length // 2

        numbersToAdd =
            ints
                |> List.take toAdd
    in
        ints ++ numbersToAdd


compute2 : Input -> Output
compute2 input =
    let
        halfway =
            input
                |> List.length
                |> (\x -> x // 2 + 1)
    in
        input
            |> normalize2
            |> List.Extra.groupsOfWithStep halfway 1
            |> List.map
                (\group ->
                    let
                        first =
                            group
                                |> List.head

                        last =
                            group
                                |> List.reverse
                                |> List.head

                        both =
                            Maybe.map2 (,) first last
                    in
                        case both of
                            Just ( a, b ) ->
                                if a == b then
                                    a
                                else
                                    0

                            Nothing ->
                                Debug.crash "weird?"
                )
            |> List.sum


tests1 : List (Test Input Output)
tests1 =
    [ Test "example 1"
        "1122"
        [ 1, 1, 2, 2 ]
        3
    , Test "example 2"
        "1111"
        [ 1, 1, 1, 1 ]
        4
    , Test "example 3"
        "1234"
        [ 1, 2, 3, 4 ]
        0
    , Test "example 4"
        "91212129"
        [ 9, 1, 2, 1, 2, 1, 2, 9 ]
        9
    ]


tests2 : List (Test Input Output)
tests2 =
    [ Test "example 1"
        "1212"
        [ 1, 2, 1, 2 ]
        6
    , Test "example 2"
        "1221"
        [ 1, 2, 2, 1 ]
        0
    , Test "example 3"
        "123425"
        [ 1, 2, 3, 4, 2, 5 ]
        4
    , Test "example 4"
        "123123"
        [ 1, 2, 3, 1, 2, 3 ]
        12
    , Test "example 5"
        "12131415"
        [ 1, 2, 1, 3, 1, 4, 1, 5 ]
        4
    ]
