module Year2018.Day02 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)
import Dict.Extra



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List String


type alias Input2 =
    List String


type alias Output1 =
    Int


type alias Output2 =
    String



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    String.lines string


parse2 : String -> Input2
parse2 string =
    String.lines string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    let
        two : Int
        two =
            input
                |> List.filter (has 2)
                |> List.length

        three : Int
        three =
            input
                |> List.filter (has 3)
                |> List.length

        has : Int -> String -> Bool
        has n string =
            let
                counts =
                    string
                        |> String.toList
                        |> Dict.Extra.frequencies
            in
            Dict.Extra.any (\k v -> v == n) counts
    in
    two * three


compute2 : Input2 -> Output2
compute2 input =
    let
        differByOne : String -> String -> Bool
        differByOne x y =
            let
                pairedChars =
                    List.map2 Tuple.pair
                        (String.toList x)
                        (String.toList y)

                diffCount =
                    List.foldl
                        (\( charX, charY ) difference ->
                            if charX == charY then
                                difference

                            else
                                difference + 1
                        )
                        0
                        pairedChars
            in
            diffCount == 1

        ( wantedA, wantedB ) =
            input
                |> List.foldl
                    (\stringA found ->
                        if found == Nothing then
                            let
                                oneDiffs =
                                    -- inefficient...
                                    input
                                        |> List.filter (differByOne stringA)
                            in
                            case oneDiffs of
                                [ stringB ] ->
                                    Just ( stringA, stringB )

                                _ ->
                                    Nothing

                        else
                            found
                    )
                    Nothing
                |> Advent.unsafeMaybe

        common =
            List.map2 Tuple.pair
                (String.toList wantedA)
                (String.toList wantedB)
                |> List.filter (\( a, b ) -> a == b)
                |> List.map Tuple.first
                |> String.fromList
    in
    common



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example"
        "input"
        -1
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
ivyhczwokexltwhsfamqprbnuy
ivjhcjdokexltwwsfamqpabnuy
ivjhczdokebltwgsfydqprbnuy
ivjhcadokexftogsfamqprbnuy
idjhczdokexltwgsfacqprbnuh
ivjhgzookexltwgsfamqjrbnuy
uvjhctdokexltwosfamqprbnuy
ivrhczdokexltwhzfamqprbnuy
ivjhczuxkexltwgsfamqprbney
ivjhczdokemltwgsfadnprbnuy
ifjhczdokexltwgsfamqprbkuf
ivjhkzdokltltwgsfamqprbnuy
ivjuczdhkexltwgsfamqprtnuy
ivjhjzdxkexltwgsfapqprbnuy
ivlhczdoxexltwgsfamqprgnuy
ivjhczdoknxltwgssamqsrbnuy
ivjhczdokexltwgswadqprbruy
ivjhczdokexthwgsfampprbnuy
uvjhczrozexltwgsfamqprbnuy
ivolczdokexltwgsffmqprbnuy
ivjhczibkexltwgsfamoprbnuy
ivjhczdokefltmgsfamqprbouy
ivjhczdobexltngsfamsprbnuy
ivjhczdokexltwvsfxqqprbnuy
dvjhczdokexucwgsfamqprbnuy
kvjhszkokexltwgsfamqprbnuy
ivjhczdokexrtegsfamqprbnus
ivjhctdokexltwglgamqprbnuy
ivjhczdozexutwgsfamqdrbnuy
ivjhczqokemltwgsfakqprbnuy
uvjhczdokexlqwgsfadqprbnuy
ivjhczdohexltwglffmqprbnuy
izjhczdokexltwgsfamqprbsqy
iajhczdokwxltwgjfamqprbnuy
ivjfczdokexllwgslamqprbnuy
ivjhczdoyexltwgsfamqdrbnxy
ivjhczdokekwtwssfamqprbnuy
ivjhcodokexltwgsfamqirxnuy
ihjhhzdokexltwgsfamqlrbnuy
ivjdpzdokexltwfsfamqprbnuy
ivjhcpdokexltwgsfamqqrbruy
qvjhcziokexltwgsfamqprbnny
ivohczdomexltwgsfkmqprbnuy
ivjhczhokhxlywgsfamqprbnuy
ivjhczdokexltwgmffmqprbruy
ivjhczdokqxltwgcfamqprbnyy
ivjhczdokepltwgsfamcprbnay
ivjhczdokexltwgseamqpmbnua
ivjzczdokexltwgszamqplbnuy
ivjhczpokexltwgvfgmqprbnuy
idjhczdokexltwgsxamqprbndy
ivjhczdxkexltwgcgamqprbnuy
ivjhczdckexatpgsfamqprbnuy
ivjrczdorexltwgsfamqprbnvy
ivjoczdokexltwgswamqprbtuy
iylhczdokexltwgsfamqpxbnuy
imxhczdokkxltwgsfamqprbnuy
ivvhczdoktxltwgsfamaprbnuy
ivyhczdokexltwhsfayqprbnuy
ivjhcrdokexltegsfamqprbnum
rvjhezdokexltwgsoamqprbnuy
ivjzczdokexlbwgsfkmqprbnuy
ivjhczdokelltwgsyamqprbnoy
ixjhczdorexltwgsfamqprbuuy
ivjhczpokexdtwglfamqprbnuy
ivjhczdokexltwgfgamcprbnuy
ikjhczdokexltwgsfamqirbnux
ivjhczdopjxltwgsfamqprbnny
ivchczdokexltwgniamqprbnuy
ivjhczdooeqltwgsfamqprbniy
ivjhcldonexltwgbfamqprbnuy
ixjhczdokehltwgsfamqprbnuf
ivjhczdckefltwgsfamqppbnuy
ivjhczdoqrxltwgsfamqprbnun
ivjhczdokcxltwgmfarqprbnuy
ivjhcziorexltqgsfamqprbnuy
ivjhwzdokexltwgnfamqprbcuy
ivjhczdoqexltwgsfazqprunuy
iijhczdokexltwgsyamqprbnug
ivjhczdokexltwgxfamhprbnry
ivjhczdakexltwgsfaeqlrbnuy
ivjhqzdokehltwgsfampprbnuy
ivjhczdokexltwlsfpmyprbnuy
ivjhfzdoktxltwgsfamzprbnuy
ivlhvzdokexltwgsvamqprbnuy
ivjhczdbkexltwgsaamqprfnuy
ivahcedokexltigsfamqprbnuy
cvjhczdokexltwgsfamapibnuy
ivjhczkokbxltwgsfbmqprbnuy
pvjuczdnkexltwgsfamqprbnuy
iyjhczdckexotwgsfamqprbnuy
ivjhzzdokvxltwgsfamqprbnuo
ivjhczdobexltwgsxamqprbnry
ivjhczdokexltwgsfaprprbnub
ivjhczdokexltwgofarqprbkuy
ivjhczdokexltwgbfymqprbnhy
ibjhczdokexltwgsfkmqpvbnuy
ivjhczdzkexlywgsfacqprbnuy
hvdhczdokexltwglfamqprbnuy
ivjhczdokexrtwgsfamqprbsuh
ivjhczhokexltngsfamqpjbnuy
ivjhcsjokexltwgsfaeqprbnuy
ivjmczdokexltmgsfamqpbbnuy
wvjhczdokexltwgsfamkpkbnuy
icjhpzdoaexltwgsfamqprbnuy
ivjmczdhkexltwgsfzmqprbnuy
ivjhczdokexytwgsfamqprbwug
ikjhczdjkexljwgsfamqprbnuy
ivjvcdmokexltwgsfamqprbnuy
ivjhazdorixltwgsfamqprbnuy
ivchczdokexltwgsfamzprenuy
ivjcczdokexlttgsfamqpmbnuy
ibchgzdokexltwgsfamqprbnuy
ivjhczdokepltwgsfamqpeenuy
ivjnwzdokexlrwgsfamqprbnuy
ivjhczdokexitwgsfadqlrbnuy
icjhcrdokexltwgsfamqkrbnuy
ivngczdokexltwgsfamqprbyuy
ivjhuudokexlvwgsfamqprbnuy
ivjhczdnkexltwgsfhmqpxbnuy
itjhczdokexltwvsfamgprbnuy
ivjhcddokexltwgsfakqprbnny
ivjhuzdojexltwfsfamqprbnuy
idjhczdokexltwgsfamqukbnuy
ivjhczdokexlzigsfamqprbngy
ivjwczdokexltwgufamqprbnuo
iijhczdokexltwfsfadqprbnuy
ivjhczdukexdtwgsfamqpsbnuy
idjhczdokexllwgssamqprbnuy
zvjhczdokexrtwgsfamqplbnuy
ivphczdofexltwgefamqprbnuy
ivhhczdokexlpwgsjamqprbnuy
ivjhczdovexltwgsfamqprhnuj
ivjhczdoklxltwgseamqprlnuy
ivjhcqdokexltngsfamqprdnuy
ivjhczdoifxltagsfamqprbnuy
izjhczdokexltwjsramqprbnuy
psjhczdokexlgwgsfamqprbnuy
ivjhcadokexltwgsfsmqwrbnuy
ivjhczdokexltwgsfawqiibnuy
ivjhczkokexhtwgsfamqprbnuk
ivjhcmdukexltwgsfamvprbnuy
ivjlczdokexltwgsfamquibnuy
ivjhczdokexntwgyfamqprbniy
ivjhczdokexltwlsfafqprbnuc
ivjhczdosexltrtsfamqprbnuy
ivjhcznokexbtwgsfafqprbnuy
ivwtczdotexltwgsfamqprbnuy
ivjhvzdokexltigsoamqprbnuy
ivjhcmdokexltwasfamqirbnuy
ivthczdokexltwgsfaydprbnuy
ivjhwzdskexltwgsfamqprbnus
icjhczdosuxltwgsfamqprbnuy
ivjhczdokexltwgstamqbrmnuy
iejhczuoktxltwgsfamqprbnuy
ivjhczdokeqltwgskamqprbniy
ivjhlzdokexltugsfamqprbpuy
iwjqczdckexltwgsfamqprbnuy
ivjhwzdokexluwgsfxmqprbnuy
ivjhczdokexltwgwfwmqprbguy
gvjhczkokexltwgsfgmqprbnuy
ivjhczdoyexlhwgsfamqprbnoy
cvjhczdokexltwgsfomqprinuy
vvmhczdokexltwgsfamqprbnun
vvjhczdokexltwgsftmfprbnuy
ivkhckdokhxltwgsfamqprbnuy
iyjhczdkkexltjgsfamqprbnuy
ivlhczdokexltwgsfamqyrbhuy
tvjhmzdokexltwgsfamqorbnuy
ivjhczdokexltwvsfamqprbnxi
ivjhczdowexltwgswamqerbnuy
wvjiczdomexltwgsfamqprbnuy
ivjpizdokexltwgvfamqprbnuy
ivjhuzdokexlzwgspamqprbnuy
ivjhczdokeyltwgkfamqprdnuy
jvjhczdokexlnwgsfamqirbnuy
ivjheidokexltwvsfamqprbnuy
mvjhczdokexltwgsfamqyrsnuy
ivjhazdykexltwgsramqprbnuy
ivjkcodokexltwgsxamqprbnuy
ikjhczdoktxltwgpfamqprbnuy
ivjhyzdfkexmtwgsfamqprbnuy
ivohczdokexltugsfamqprynuy
ivjkczdqkexltwgshamqprbnuy
ivjhczdokexltwgskamqynbnuy
icjhczdokexltwgofamrprbnuy
ivjhlzdokealtwgsfamqsrbnuy
ivehczdybexltwgsfamqprbnuy
ovjhczdokexltwgsfamqirbnuo
ivjoczdokexltwgsfamqurbnty
ivjmczdokexltwgsfrmqprnnuy
ivjhczdowpxltwgbfamqprbnuy
ivjhczdokexltwfsfamqkrgnuy
ivjhwzdokexltwgsfavqprbnuq
jvjhczdokexltwgsiamqprbnny
ivjhlzdouexltwfsfamqprbnuy
ivjhczdokexltwgsfamqbrbnlv
iwjhczdokexltwgsfapqprbnqy
idjhczdokexltwgsaamqrrbnuy
ivjhjzdopepltwgsfamqprbnuy
ivjmczdokejltwgsfamqpbbnuy
ivjhczdokexltwgsuamdprvnuy
injhczdokexltwgefamqurbnuy
iujhczdokexltwgsaamqjrbnuy
ivjhczdokexltwgvfaaqprbnly
ivehczdokexltwgsfamqppbnui
ivxhczdodexltwgsfamqplbnuy
ivjhczfokexltwgsfamqpwbauy
ivjhcztwkexhtwgsfamqprbnuy
ivjeczdokexltygsfmmqprbnuy
ivjhchdokexltwgsmameprbnuy
ivkhczdoklxltwggfamqprbnuy
ivjhczdzkexltwhsfamqprjnuy
ivjhcedokeultngsfamqprbnuy
ivjhczdokexvtwgseabqprbnuy
ivjhczdooexltlgsfamqpibnuy
ivjgczvosexltwgsfamqprbnuy
ivlhczwokexltwgsfamqmrbnuy
lvjhczdokexutwgsfamrprbnuy
ivahczdokexpdwgsfamqprbnuy
ivjhcznokexltwhsfamqpnbnuy
ivjhczdpkyxltwgbfamqprbnuy
ivjhnzdokexltwgsftmqprinuy
ivihczdokexltnhsfamqprbnuy
ivjhcbdokevltwgsfamqprbauy
hgjoczdokexltwgsfamqprbnuy
dvjhczdckexltwgsfamqpybnuy
ivjhcadokesltwgsfsmqwrbnuy
ivjhwzdokexlttgsfamqprbney
ivjhcidokexltwgofamqfrbnuy
ivokwzdokexltwgsfamqprbnuy
ivjiczdokexltwgsfaqqarbnuy
ivjhczdokexqtwfsfamgprbnuy
ivjhczdokealtwgsfamqerbnqy
ivjhczdskexltwgsfamqprznuu
ivjhwzdokexltwjsfdmqprbnuy
ivjhczaokexlzwgsfamqprbnus
ivjhczdokexltwosfamqnrbnux
ivjhczdokexlqwgsfamwprcnuy
ivjhczdqkexltwgswamqpcbnuy
ijjhczdokexnttgsfamqprbnuy
ivjhcedckexltwgsfamqprbnpy
ivjhczdokeyltwgsfamqshbnuy
ivjhczdokexltsgsfamqpmznuy
ivjlczdtkeiltwgsfamqprbnuy
ivjhczdokexltwgsfkmtprbnby
ivjhnzdozexltwgsfamqprbnuc
xqjxczdokexltwgsfamqprbnuy
ivjhczdokeyltwgsfamqnrbnuw
ivjwczgokexltwgsfamvprbnuy
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
