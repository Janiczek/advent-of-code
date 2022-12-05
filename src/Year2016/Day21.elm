module Year2016.Day21 exposing (..)

--module Year2016.Day21 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent exposing (Test)
import List.Extra as List
import Result.Extra as Result



-- 1. TYPES (what is the best representation of the problem?)


type Instruction
    = SwapPos Pos Pos
    | SwapLetter Char Char
    | RotateLeft Int
    | RotateRight Int
    | RotateBasedOnLetter Char
    | ReverseBetween Pos Pos
    | Move Pos Pos


type alias Pos =
    Int


type alias Input1 =
    List Instruction


type alias Input2 =
    List Instruction


type alias Output1 =
    String


type alias Output2 =
    String



-- 2. PARSE (mangle the input string into the representation we decided on)


init1 : List Char
init1 =
    String.toList "abcdefgh"


init2 : List Char
init2 =
    String.toList "fbgdceah"


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> Result.combineMap parseLine
        |> Advent.unsafeResult "parse1"


parseLine : String -> Result String Instruction
parseLine line =
    Result.fromMaybe line <|
        case String.words line of
            [ "swap", "position", x, "with", "position", y ] ->
                Maybe.map2 SwapPos (String.toInt x) (String.toInt y)

            [ "swap", "letter", x, "with", "letter", y ] ->
                Maybe.map2 SwapLetter (toChar x) (toChar y)

            [ "rotate", "left", x, "step" ] ->
                Maybe.map RotateLeft (String.toInt x)

            [ "rotate", "left", x, "steps" ] ->
                Maybe.map RotateLeft (String.toInt x)

            [ "rotate", "right", x, "step" ] ->
                Maybe.map RotateRight (String.toInt x)

            [ "rotate", "right", x, "steps" ] ->
                Maybe.map RotateRight (String.toInt x)

            [ "rotate", "based", "on", "position", "of", "letter", x ] ->
                Maybe.map RotateBasedOnLetter (toChar x)

            [ "reverse", "positions", x, "through", y ] ->
                Maybe.map2 ReverseBetween (String.toInt x) (String.toInt y)

            [ "move", "position", x, "to", "position", y ] ->
                Maybe.map2 Move (String.toInt x) (String.toInt y)

            _ ->
                Nothing


toChar : String -> Maybe Char
toChar string =
    string
        |> String.toList
        |> List.head


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    List.foldl eval init1 input
        |> String.fromList


eval : Instruction -> List Char -> List Char
eval instr str =
    case instr of
        SwapPos x y ->
            swapPos x y str

        SwapLetter x y ->
            swapLetter x y str

        RotateLeft n ->
            rotateLeft n str

        RotateRight n ->
            rotateRight n str

        RotateBasedOnLetter x ->
            rotateBasedOnLetter x str

        ReverseBetween min_ max_ ->
            reverseBetween min_ max_ str

        Move x y ->
            move x y str


move : Int -> Int -> List Char -> List Char
move x y str =
    let
        -- (1,3)
        -- A B C D E F
        -- 0 1 2 3 4 5
        --   ^   ^
        -- A   C D E F
        -- A C D   E F
        -- A C D B E F
        --------------
        -- (3,1)
        -- A B C D E F
        -- 0 1 2 3 4 5
        --   ^   ^
        -- A B C   E F
        -- A   B C E F
        -- A D B C E F
        x_ =
            List.take 1 (List.drop x str)

        leftOfX =
            List.take x str

        rightOfX =
            List.drop (x + 1) str

        withoutX =
            leftOfX ++ rightOfX

        beforeY =
            List.take y withoutX

        afterY =
            List.drop y withoutX
    in
    beforeY ++ x_ ++ afterY


reverseMove : Int -> Int -> List Char -> List Char
reverseMove x y str =
    move y x str


reverseBetween : Int -> Int -> List Char -> List Char
reverseBetween min_ max_ str =
    let
        between =
            List.take (max_ - min_ + 1) (List.drop min_ str)

        reversed =
            List.reverse between

        before =
            List.take min_ str

        after =
            List.drop (max_ + 1) str
    in
    before ++ reversed ++ after


rotateBasedOnLetter : Char -> List Char -> List Char
rotateBasedOnLetter x str =
    let
        posX =
            List.findIndex ((==) x) str |> Advent.unsafeMaybe "rotate on x"

        n =
            1
                + posX
                + (if posX >= 4 then
                    1

                   else
                    0
                  )
    in
    rotateRight n str


reverseRotateBasedOnLetter : Char -> List Char -> List Char
reverseRotateBasedOnLetter x str =
    let
        posX =
            List.findIndex ((==) x) str |> Advent.unsafeMaybe "rotate on x"

        n =
            case posX of
                1 ->
                    1

                3 ->
                    2

                5 ->
                    3

                7 ->
                    4

                2 ->
                    6

                4 ->
                    7

                6 ->
                    0

                0 ->
                    1

                _ ->
                    0
    in
    rotateLeft n str


swapLetter : Char -> Char -> List Char -> List Char
swapLetter x y str =
    let
        posX =
            List.findIndex ((==) x) str |> Advent.unsafeMaybe (String.fromList str ++ ", swap letter " ++ String.fromChar x)

        posY =
            List.findIndex ((==) y) str |> Advent.unsafeMaybe (String.fromList str ++ ", swap letter " ++ String.fromChar y)
    in
    swapPos posX posY str


rotateRight : Int -> List Char -> List Char
rotateRight n str =
    let
        len =
            List.length str

        newN =
            len - modBy len n
    in
    rotateLeft newN str


rotateLeft : Int -> List Char -> List Char
rotateLeft n str =
    let
        -- A B C D E F
        -- 0 1 2 3 4 5
        --     ^
        -- C D E F A B
        len =
            List.length str

        n_ =
            modBy len n

        before =
            List.take n_ str

        after =
            List.drop n_ str
    in
    after ++ before


swapPos : Int -> Int -> List Char -> List Char
swapPos x y str =
    let
        min_ =
            min x y

        max_ =
            max x y

        min__ =
            List.take 1 (List.drop min_ str)

        max__ =
            List.take 1 (List.drop max_ str)

        beforeMin =
            List.take min_ str

        -- A B C D E F
        -- 0 1 2 3 4 5
        --   ^     ^
        -- drop 2, take 2
        -- drop (min + 1), take (max - min - 1)
        between =
            List.take (max_ - min_ - 1) (List.drop (min_ + 1) str)

        afterMax =
            List.drop (max_ + 1) str
    in
    beforeMin ++ max__ ++ between ++ min__ ++ afterMax


compute2 : Input2 -> Output2
compute2 input =
    -- wrong: bdcfheag
    -- wrong: egchfdab
    List.foldr evalReverse init2 input
        |> String.fromList


evalReverse : Instruction -> List Char -> List Char
evalReverse instr str =
    case instr of
        SwapPos x y ->
            swapPos x y str

        SwapLetter x y ->
            swapLetter x y str

        RotateLeft n ->
            rotateRight n str

        RotateRight n ->
            rotateLeft n str

        RotateBasedOnLetter x ->
            reverseRotateBasedOnLetter x str

        ReverseBetween min_ max_ ->
            reverseBetween min_ max_ str

        Move x y ->
            reverseMove x y str



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example"
                ("""
        swap position 4 with position 0
        swap letter d with letter b
        reverse positions 0 through 4
        rotate left 1 step
        move position 1 to position 4
        move position 3 to position 0
        rotate based on position of letter b
        rotate based on position of letter d
        """
                    |> Advent.removeNewlinesAtEnds
                )
                Nothing
                "decab"
     -}
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
rotate based on position of letter a
swap letter g with letter d
move position 1 to position 5
reverse positions 6 through 7
move position 5 to position 4
rotate based on position of letter b
reverse positions 6 through 7
swap letter h with letter f
swap letter e with letter c
reverse positions 0 through 7
swap position 6 with position 4
rotate based on position of letter e
move position 2 to position 7
swap position 6 with position 4
rotate based on position of letter e
reverse positions 2 through 3
rotate right 2 steps
swap position 7 with position 1
move position 1 to position 2
move position 4 to position 7
move position 5 to position 0
swap letter e with letter f
move position 4 to position 7
reverse positions 1 through 7
rotate based on position of letter g
move position 7 to position 4
rotate right 6 steps
rotate based on position of letter g
reverse positions 0 through 5
reverse positions 0 through 7
swap letter c with letter f
swap letter h with letter f
rotate right 7 steps
rotate based on position of letter g
rotate based on position of letter c
swap position 1 with position 4
move position 7 to position 3
reverse positions 2 through 6
move position 7 to position 0
move position 7 to position 1
move position 6 to position 7
rotate right 5 steps
reverse positions 0 through 6
move position 1 to position 4
rotate left 3 steps
swap letter d with letter c
move position 4 to position 5
rotate based on position of letter f
rotate right 1 step
move position 7 to position 6
swap position 6 with position 0
move position 6 to position 2
rotate right 1 step
swap position 1 with position 6
move position 2 to position 6
swap position 2 with position 1
reverse positions 1 through 7
move position 4 to position 1
move position 7 to position 0
swap position 6 with position 7
rotate left 1 step
reverse positions 0 through 4
rotate based on position of letter c
rotate based on position of letter b
move position 2 to position 1
rotate right 0 steps
swap letter b with letter d
swap letter f with letter c
swap letter d with letter a
swap position 7 with position 6
rotate right 0 steps
swap position 0 with position 3
swap position 2 with position 5
swap letter h with letter f
reverse positions 2 through 3
rotate based on position of letter c
rotate left 2 steps
move position 0 to position 5
swap position 2 with position 3
rotate right 1 step
rotate left 2 steps
move position 0 to position 4
rotate based on position of letter c
rotate based on position of letter g
swap position 3 with position 0
rotate right 3 steps
reverse positions 0 through 2
move position 1 to position 2
swap letter e with letter c
rotate right 7 steps
move position 0 to position 7
rotate left 2 steps
reverse positions 0 through 4
swap letter e with letter b
reverse positions 2 through 7
rotate right 5 steps
swap position 2 with position 4
swap letter d with letter g
reverse positions 3 through 4
reverse positions 4 through 5
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
