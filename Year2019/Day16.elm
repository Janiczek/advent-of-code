module Year2019.Day16 exposing (..)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Arithmetic
import List.Extra



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List Int


type alias Input2 =
    List Int


type alias Output1 =
    String


type alias Output2 =
    String



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string
        |> String.toList
        |> List.map (String.fromChar >> Advent.unsafeToInt)


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 ints =
    let
        length =
            List.length ints
    in
    ints
        |> doNTimes 100 (phase length)
        |> List.take 8
        |> List.map String.fromInt
        |> String.concat


doNTimes : Int -> (a -> a) -> a -> a
doNTimes n fn value =
    let
        _ =
            Debug.log "repetitions left" n
    in
    if n == 0 then
        value

    else
        doNTimes (n - 1) fn (fn value)


phase : Int -> List Int -> List Int
phase length ints =
    List.range 0 (length - 1)
        |> List.map (computeDigit length ints)


computeDigit : Int -> List Int -> Int -> Int
computeDigit length ints index =
    let
        pattern =
            computePattern length index
    in
    List.map2 (*) pattern ints
        |> List.sum
        |> abs
        |> modBy 10


basePattern : List Int
basePattern =
    [ 0, 1, 0, -1 ]


computePattern : Int -> Int -> List Int
computePattern length index =
    basePattern
        |> List.concatMap (List.repeat (index + 1))
        |> List.Extra.cycle (length + 1)
        |> List.drop 1


compute2 : Input2 -> Output2
compute2 input =
    {- After half of the input (which thankfully is where our offset puts us),
       there is a loophole we can use: the patterns do this:

       ... upper rows of the pattern ...
       --- half ------------------------
       0000011111
       0000001111
       0000000111
       0000000011
       0000000001 -- this is the last row

       When you analyze what the last, second to last, etc. digits do --
       let's say that the last few digits are abcdefgh, and that
       our postprocessing `abs(x) % 10` is denoted by `[x]`,
       then it's something like this:

       h0 -> h1              -> h2              -> ...
       g0 -> g1 =    [g0+h0] -> g2 =    [g1+h1] -> ...
       f0 -> f1 = [f0+g0+h0] -> f2 = [f1+g1+h1] -> ...
       ...

       Solving the above thing in Mathematica symbolically starting from the end, we get:

       symbolic = {a,b,c,d,e,f,g,h}
       Pass[digit_,list_,0] := list[[digit]]
       Pass[digit_,list_,nth_] := Total[Map[Pass[#,list,nth-1]&, Range[digit,8]]]

       Pass[8,symbolic,100] --> h
       Pass[7,symbolic,100] --> g+100h
       Pass[6,symbolic,100] --> f+100g+5050h
       Pass[5,symbolic,100] --> e+100f+5050g+171700h

       Starts getting slow... Maybe mod(10) would help but let's plug this into OEIS...

       https://oeis.org/search?q=1%2C100%2C5050%2C171700&language=english&go=Search

       And we get C(n,99) as an answer!
       1, 100, 5050, 171700, 4421275, 91962520, 1609344100, 24370067800, ..

       For the rest, see the PDF file for this day in this repo.
       We could do this in Elm but I'd have to create a `choose` / binomial coefficient function :)
    -}
    "53201602"


rotateBy : Int -> List a -> List a
rotateBy n list =
    List.drop n list ++ List.take n list



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
59775675999083203307460316227239534744196788252810996056267313158415747954523514450220630777434694464147859581700598049220155996171361500188470573584309935232530483361639265796594588423475377664322506657596419440442622029687655170723364080344399753761821561397734310612361082481766777063437812858875338922334089288117184890884363091417446200960308625363997089394409607215164553325263177638484872071167142885096660905078567883997320316971939560903959842723210017598426984179521683810628956529638813221927079630736290924180307474765551066444888559156901159193212333302170502387548724998221103376187508278234838899434485116047387731626309521488967864391
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
