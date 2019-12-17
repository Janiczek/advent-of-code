module Year2019.Day16 exposing (..)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import List.Extra
import List.Zipper as Zipper exposing (Zipper)



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

        range =
            List.range 0 (length - 1)
    in
    ints
        |> Zipper.fromList
        |> Advent.unsafeMaybe "compute1 empty list"
        |> doNTimes 100 (phase range length)
        |> Zipper.toList
        |> List.take 8
        |> List.map String.fromInt
        |> String.concat



--"25131128"


doNTimes : Int -> (a -> a) -> a -> a
doNTimes n fn value =
    if n == 0 then
        value

    else
        doNTimes (n - 1) fn (fn value)


phase : List Int -> Int -> Zipper Int -> Zipper Int
phase range length ints =
    let
        _ =
            Debug.log "\\o/" ()
    in
    range
        |> List.map (computeDigit length ints)
        |> Zipper.fromList
        |> Advent.unsafeMaybe "empty result list in phase"


computeDigit : Int -> Zipper Int -> Int -> Int
computeDigit length zipper index =
    patternFn length index zipper
        |> abs
        |> modBy 10


patternFn : Int -> Int -> Zipper Int -> Int
patternFn length index zipper =
    let
        _ =
            Debug.log "digit" index
    in
    let
        index1 =
            index + 1

        -- current and before == "saved"
        positive =
            zipper
                |> Debug.log "starting"
                |> drop index
                |> Debug.log "dropped the first irregular 0s"
                |> jumpAndDrop index1 (3 * index1)
                |> Debug.log "after everything"
                |> Zipper.toList
                |> List.sum

        negative =
            0

        {- zipper
           |> drop (3 * index1 - 1)
           |> jumpAndDrop index1 (3 * index1)
           |> Zipper.toList
           --|> Debug.log "keep -"
           |> List.sum
        -}
    in
    positive - negative


jumpAndDrop : Int -> Int -> Zipper Int -> Zipper Int
jumpAndDrop jumpAmount dropAmount zipper =
    if Zipper.isLast zipper then
        zipper

    else
        jumpAndDrop
            jumpAmount
            dropAmount
            (zipper
                |> jumpForwards jumpAmount
                |> Debug.log ("after keeping " ++ String.fromInt jumpAmount)
                |> drop dropAmount
                |> Debug.log ("after dropping " ++ String.fromInt dropAmount)
            )


jumpForwards : Int -> Zipper Int -> Zipper Int
jumpForwards n zipper =
    doNTimes n
        (Zipper.next >> Maybe.withDefault zipper)
        zipper


drop : Int -> Zipper Int -> Zipper Int
drop n zipper =
    zipper
        |> Zipper.previous
        |> Maybe.withDefault zipper
        |> Zipper.mapAfter (List.drop n)
        |> Zipper.next


compute2 : Input2 -> Output2
compute2 input =
    let
        skip =
            input
                |> List.take 7
                |> List.map String.fromInt
                |> String.concat
                |> Advent.unsafeToInt

        realInput =
            input
                |> List.repeat 10000
                |> List.concat
                -- ????????
                |> Zipper.fromList
                |> Advent.unsafeMaybe "empty realInput"

        length =
            List.length input * 10000

        range =
            List.range 0 (length - 1)
    in
    realInput
        |> doNTimes 100 (phase range length)
        |> drop skip
        |> Zipper.toList
        |> List.take 8
        |> List.map String.fromInt
        |> String.concat



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
