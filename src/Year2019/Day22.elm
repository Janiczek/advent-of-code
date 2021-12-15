module Year2019.Day22 exposing (..)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Arithmetic
import Bitwise



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List Instruction


type alias Input2 =
    List Instruction


type alias Output1 =
    Int


type alias Output2 =
    Int


type alias Deck =
    { items : List Int
    , length : Int
    }


type Instruction
    = DealNew
    | Cut Int
    | DealWithIncrement Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> List.map parseLine


parseLine : String -> Instruction
parseLine string =
    case String.words string of
        [ "deal", "with", "increment", n ] ->
            DealWithIncrement <| Advent.unsafeToInt n

        [ "deal", "into", "new", "stack" ] ->
            DealNew

        [ "cut", n ] ->
            Cut <| Advent.unsafeToInt n

        _ ->
            Debug.todo "parseLine wat"


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


dealNew : Deck -> Deck
dealNew deck =
    { deck | items = List.reverse deck.items }


cut : Int -> Deck -> Deck
cut n deck =
    if n == 0 then
        deck

    else if n < 0 then
        { deck | items = List.drop (deck.length + n) deck.items ++ List.take (deck.length + n) deck.items }

    else
        { deck | items = List.drop n deck.items ++ List.take n deck.items }


dealWithIncrement : Int -> Deck -> Deck
dealWithIncrement increment deck =
    { deck
        | items =
            deck.items
                |> List.indexedMap (\i n -> ( modBy deck.length (i * increment), n ))
                |> List.sortBy Tuple.first
                |> List.map Tuple.second
    }


deckOfLength : Int -> Deck
deckOfLength n =
    { length = n
    , items = List.range 0 (n - 1)
    }


compute1 : Input1 -> Output1
compute1 instructions =
    deckOfLength 10007
        |> processMany1 instructions
        |> .items
        |> List.indexedMap Tuple.pair
        |> List.filter (\( i, n ) -> n == 2019)
        |> List.head
        |> Advent.unsafeMaybe "compute1 head"
        |> Tuple.first


processMany1 : List Instruction -> Deck -> Deck
processMany1 instructions deck =
    List.foldl process1 deck instructions


process1 : Instruction -> Deck -> Deck
process1 instruction deck =
    case instruction of
        DealNew ->
            dealNew deck

        Cut n ->
            cut n deck

        DealWithIncrement n ->
            dealWithIncrement n deck


findLinearEquationInMathematica : Int -> List Instruction -> String
findLinearEquationInMathematica m instructions =
    -- a bit of cheating with Mathematica
    -- full simplification of the output of this function gives
    -- Mod[115490606888493+18870914573696 p,119315717514047]
    let
        length =
            String.fromInt m
    in
    "Mod["
        ++ List.foldl
            (process2 length)
            "p"
            instructions
        ++ ","
        ++ length
        ++ "]"


compute2 : Input2 -> Output2
compute2 instructions =
    let
        _ =
            findLinearEquationInMathematica 119315717514047 instructions
                |> Debug.log "plug this into Mathematica to get the equation for doing the shuffle 1x"
    in
    -- Using `findLinearEquationInMathematica instructions`, we have got our
    -- formula for Mathematica, and then simplified it there:
    --
    -- f(x) = (18870914573696 x + 115490606888493) mod 119315717514047
    --
    -- Now let's use the formula for repeated linear equation application.
    -- Theoretically we can do it in this way:
    --
    -- f^n(x) = a^n * x + ((a^n)-1)/(a-1) * b
    --
    -- But I've had problems doing the exponentiation in the `((a^n)-1)/(a-1)`.
    -- So let's do something similar to exponentiation by squaring instead:
    -- just substitute numbers' `x * y` with functions' `x << y`.
    --
    -- Aaaaand because Elm is bonkers wrt. signed/unsigned integers,
    -- 32 vs 64bit range, etc., I couldn't get mulMod working reliably.
    -- Had to use Python in the end. See Day22Part2.py
    --
    -- After all that, we got a' = 80484954784936, b' = 49323001031774.
    -- Now we have to find x for which f(x) = a'x + b = 2020 (mod m)
    -- Simple arithmetic and modular inverse should be enough...
    --
    -- a' * x + b' = 2020                            (mod m)
    -- a' * x      = 2020 - b'                       (mod m)
    -- a' * x      = -49323001029754                 (mod m) -- literally use the modulo
    -- a' * x      = 69992716484293                  (mod m)
    -- x           = 69992716484293 * 1/a'           (mod m) -- here we use Arithmetic.modularInverse
    -- x           = 69992716484293 * 28052063120404 (mod m) -- here we use mulMod
    -- x           = 45347150615590
    45347150615590


process2 : String -> Instruction -> String -> String
process2 length instruction position =
    case instruction of
        DealNew ->
            "(" ++ length ++ "-" ++ position ++ "-1)"

        Cut n ->
            "(" ++ position ++ "-(" ++ String.fromInt n ++ "))"

        DealWithIncrement n ->
            "(" ++ position ++ "*" ++ String.fromInt n ++ ")"



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
deal with increment 61
cut 7724
deal into new stack
cut -7151
deal with increment 22
deal into new stack
deal with increment 11
cut -2506
deal with increment 14
cut 9670
deal with increment 59
cut 3341
deal into new stack
cut 9816
deal with increment 3
cut -7547
deal with increment 31
cut 7178
deal into new stack
deal with increment 52
deal into new stack
deal with increment 70
cut 3702
deal with increment 62
cut -6554
deal with increment 68
cut 1356
deal with increment 58
cut -9486
deal with increment 5
cut 3969
deal into new stack
deal with increment 9
cut 1376
deal with increment 70
cut 4921
deal with increment 38
deal into new stack
cut -4708
deal with increment 56
deal into new stack
cut 6672
deal with increment 53
cut -6567
deal with increment 28
cut -6494
deal with increment 57
deal into new stack
cut 3002
deal with increment 53
cut 5450
deal with increment 5
cut 7672
deal with increment 63
cut -9864
deal with increment 66
cut 5734
deal with increment 23
cut 9172
deal with increment 8
cut 3219
deal with increment 49
cut -975
deal with increment 52
deal into new stack
deal with increment 10
cut 6050
deal with increment 68
deal into new stack
cut -3778
deal with increment 25
cut 9259
deal with increment 41
cut -268
deal with increment 44
deal into new stack
cut -1431
deal with increment 48
cut -1885
deal with increment 75
cut 8570
deal with increment 49
deal into new stack
deal with increment 62
deal into new stack
deal with increment 35
deal into new stack
deal with increment 30
cut -3800
deal with increment 4
deal into new stack
deal with increment 27
cut 2827
deal with increment 2
cut -2346
deal with increment 19
cut 6615
deal with increment 38
cut 2739
deal into new stack
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
