module Year2015.Day19 exposing (Input, Output, Rule, applyRules, compute1, compute2, go, input, main, parse, parseRule, tests1, tests2)

import Advent exposing (Test)
import List.Extra
import Set exposing (Set)


main : Program () ( Output, Output ) Never
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
    ( String, List Rule )


type alias Rule =
    ( String, String )


type alias Output =
    Int


parse : String -> Input
parse input_ =
    case
        input_
            |> String.lines
            |> List.reverse
    of
        start :: _ :: rules ->
            ( start, rules |> List.map parseRule |> List.reverse )

        _ ->
            Debug.todo "wrong input!"


parseRule : String -> Rule
parseRule string =
    case String.split " => " string of
        [ a, b ] ->
            ( a, b )

        _ ->
            Debug.todo "wrong input!"


compute1 : Input -> Output
compute1 ( start, rules ) =
    start
        |> applyRules rules
        |> Set.fromList
        |> Set.size


applyRules : List Rule -> String -> List String
applyRules rules string =
    rules
        |> List.concatMap
            (\( input_, output ) ->
                let
                    splitted =
                        string
                            -- ["", "O", ""]
                            |> String.split input_
                in
                List.range 1 (List.length splitted - 1)
                    |> List.map
                        (\i ->
                            let
                                before =
                                    splitted
                                        |> List.take i
                                        |> String.join input_

                                after =
                                    splitted
                                        |> List.drop i
                                        |> String.join input_
                            in
                            before ++ output ++ after
                        )
            )


compute2 : Input -> Output
compute2 ( target, rules ) =
    let
        invertedRules =
            rules |> List.map (\( i, o ) -> ( o, i ))
    in
    go invertedRules [ target ] "e" 0


go : List Rule -> List String -> String -> Int -> Int
go rules currents target stepsTaken =
    let
        _ =
            Debug.log "steps, length" ( stepsTaken, List.length currents )
    in
    if currents |> List.member target then
        stepsTaken

    else
        let
            allNewCurrents =
                currents
                    |> List.concatMap (applyRules rules)

            lenOfShortest =
                allNewCurrents
                    |> List.map String.length
                    |> List.minimum
                    |> Advent.unsafeMaybe
                    |> Debug.log "length of shortest"

            shortestCurrents =
                allNewCurrents
                    |> List.filter (\c -> String.length c == lenOfShortest)
                    |> List.Extra.unique
        in
        go rules shortestCurrents target (stepsTaken + 1)


tests1 : List (Test Input Output)
tests1 =
    [ Test "example"
        """H => HO
H => OH
O => HH

HOH"""
        ( "HOH"
        , [ ( "H", "HO" )
          , ( "H", "OH" )
          , ( "O", "HH" )
          ]
        )
        4
    ]


tests2 : List (Test Input Output)
tests2 =
    []


input : String
input =
    """Al => ThF
Al => ThRnFAr
B => BCa
B => TiB
B => TiRnFAr
Ca => CaCa
Ca => PB
Ca => PRnFAr
Ca => SiRnFYFAr
Ca => SiRnMgAr
Ca => SiTh
F => CaF
F => PMg
F => SiAl
H => CRnAlAr
H => CRnFYFYFAr
H => CRnFYMgAr
H => CRnMgYFAr
H => HCa
H => NRnFYFAr
H => NRnMgAr
H => NTh
H => OB
H => ORnFAr
Mg => BF
Mg => TiMg
N => CRnFAr
N => HSi
O => CRnFYFAr
O => CRnMgAr
O => HP
O => NRnFAr
O => OTi
P => CaP
P => PTi
P => SiRnFAr
Si => CaSi
Th => ThCa
Ti => BP
Ti => TiTi
e => HF
e => NAl
e => OMg

ORnPBPMgArCaCaCaSiThCaCaSiThCaCaPBSiRnFArRnFArCaCaSiThCaCaSiThCaCaCaCaCaCaSiRnFYFArSiRnMgArCaSiRnPTiTiBFYPBFArSiRnCaSiRnTiRnFArSiAlArPTiBPTiRnCaSiAlArCaPTiTiBPMgYFArPTiRnFArSiRnCaCaFArRnCaFArCaSiRnSiRnMgArFYCaSiRnMgArCaCaSiThPRnFArPBCaSiRnMgArCaCaSiThCaSiRnTiMgArFArSiThSiThCaCaSiRnMgArCaCaSiRnFArTiBPTiRnCaSiAlArCaPTiRnFArPBPBCaCaSiThCaPBSiThPRnFArSiThCaSiThCaSiThCaPTiBSiRnFYFArCaCaPRnFArPBCaCaPBSiRnTiRnFArCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFArCaSiRnFArRnCaCaCaFArSiRnFArTiRnPMgArF"""
