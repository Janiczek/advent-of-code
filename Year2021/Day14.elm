module Year2021.Day14 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    ( Dict String Int, Dict String ( String, String ) )


type alias Input2 =
    ( Dict String Int, Dict String ( String, String ) )


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    case String.split "\n\n" string of
        [ template, rules ] ->
            ( template
                |> String.toList
                |> List.groupsOfWithStep 2 1
                |> List.map String.fromList
                |> Dict.frequencies
            , parseRules rules
            )

        _ ->
            Debug.todo "parse1"


parseRules : String -> Dict String ( String, String )
parseRules string =
    string
        |> String.lines
        |> List.map parseLine
        |> Dict.fromList


parseLine : String -> ( String, ( String, String ) )
parseLine string =
    case String.split " -> " string of
        [ from, to ] ->
            let
                to_ =
                    to
                        |> String.toList
                        |> List.head
                        |> Advent.unsafeMaybe "parseLine 3"

                newTo =
                    case String.toList from of
                        [ f1, f2 ] ->
                            ( String.fromList [ f1, to_ ]
                            , String.fromList [ to_, f2 ]
                            )

                        _ ->
                            Debug.todo "parseLine 2"
            in
            ( from, newTo )

        _ ->
            Debug.todo "parseLine 1"


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


maxMinusMin : Int -> Input1 -> Output1
maxMinusMin n input =
    let
        ( tupleFreqs, _ ) =
            Advent.doNTimes n step input

        freqsList =
            Dict.toList tupleFreqs
    in
    let
        pick : (String -> String) -> List Int
        pick fn =
            freqsList
                |> List.gatherEqualsBy (Tuple.first >> fn)
                |> List.map
                    (\( x, xs ) ->
                        (x :: xs)
                            |> List.map Tuple.second
                            |> List.sum
                    )

        left =
            pick (String.left 1)

        right =
            pick (String.right 1)

        freqs =
            List.map2 max left right

        min_ =
            freqs
                |> List.minimum
                |> Advent.unsafeMaybe "min"

        max_ =
            freqs
                |> List.maximum
                |> Advent.unsafeMaybe "max"
    in
    max_ - min_


step : Input1 -> Input1
step ( current, rules ) =
    let
        new : Dict String Int
        new =
            current
                |> Dict.filter (\k v -> v > 0)
                |> Dict.toList
                |> List.concatMap
                    (\( string, count ) ->
                        let
                            ( new1, new2 ) =
                                Dict.get string rules
                                    |> Advent.unsafeMaybe "newPairs"
                        in
                        [ ( new1, count )
                        , ( new2, count )
                        ]
                    )
                |> List.foldl
                    (\( string, count ) acc ->
                        Dict.update
                            string
                            (Maybe.withDefault 0 >> (+) count >> Just)
                            acc
                    )
                    Dict.empty
    in
    ( new, rules )


compute1 : Input1 -> Output1
compute1 input =
    maxMinusMin 10 input


compute2 : Input2 -> Output2
compute2 input =
    maxMinusMin 40 input



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "example"
        """NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"""
        Nothing
        1588
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
KFVHFSSVNCSNHCPCNPVO

KS -> O
SP -> V
OH -> F
VC -> P
BO -> S
CV -> H
FO -> N
KV -> V
OV -> B
NB -> K
FS -> F
KB -> N
HK -> C
VP -> B
SV -> S
FP -> P
BS -> B
BP -> K
OS -> K
PB -> C
HB -> H
VN -> S
FB -> C
OC -> N
OO -> F
PC -> O
FK -> K
OP -> V
BH -> C
NP -> C
KF -> H
SK -> F
HN -> O
CB -> O
SN -> N
VF -> S
KC -> H
HF -> V
NC -> P
BN -> F
KO -> C
PS -> B
HO -> S
CH -> O
KP -> K
VK -> V
BB -> V
BF -> P
CS -> K
CN -> H
PK -> C
SH -> O
BC -> H
FN -> N
BK -> N
PN -> B
PO -> O
SC -> S
NO -> S
KN -> O
VB -> C
SF -> H
FH -> C
FF -> B
VO -> S
PH -> F
CK -> B
FC -> P
VV -> F
VH -> O
OF -> O
HP -> K
CO -> V
VS -> V
SB -> F
SS -> K
CF -> O
OK -> V
ON -> B
NS -> H
SO -> B
NV -> V
NH -> B
NN -> K
KH -> H
FV -> B
KK -> N
OB -> F
NK -> F
CC -> S
PP -> B
PF -> H
HC -> P
PV -> F
BV -> N
NF -> N
HV -> S
HH -> C
HS -> O
CP -> O
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
