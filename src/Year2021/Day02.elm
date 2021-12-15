module Year2021.Day02 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List Command


type alias Input2 =
    List Command


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


type Command
    = Forward Int
    | Up Int
    | Down Int


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> List.filterMap parseLine


parseLine : String -> Maybe Command
parseLine string =
    case String.split " " string of
        [ "forward", n ] ->
            Maybe.map Forward (String.toInt n)

        [ "up", n ] ->
            Maybe.map Up (String.toInt n)

        [ "down", n ] ->
            Maybe.map Down (String.toInt n)

        _ ->
            Nothing


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    input
        |> List.foldl eval1 ( 0, 0 )
        |> (\( x, y ) -> x * y)


eval1 : Command -> ( Int, Int ) -> ( Int, Int )
eval1 cmd ( x, y ) =
    case cmd of
        Forward n ->
            ( x + n, y )

        Up n ->
            ( x, y - n )

        Down n ->
            ( x, y + n )


compute2 : Input2 -> Output2
compute2 input =
    input
        |> List.foldl eval2 ( 0, 0, 0 )
        |> (\( x, y, _ ) -> x * y)


eval2 : Command -> ( Int, Int, Int ) -> ( Int, Int, Int )
eval2 cmd ( x, y, aim ) =
    case cmd of
        Forward n ->
            ( x + n, y + n * aim, aim )

        Up n ->
            ( x, y, aim - n )

        Down n ->
            ( x, y, aim + n )



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
forward 9
down 8
down 2
down 4
up 8
forward 8
down 7
forward 3
down 8
down 9
forward 7
down 3
forward 2
down 6
forward 8
down 3
down 4
up 2
forward 1
up 7
forward 7
forward 4
forward 4
down 7
forward 2
forward 7
forward 4
up 5
forward 4
forward 8
forward 8
down 5
forward 2
forward 8
forward 8
up 1
down 4
down 3
down 1
up 9
down 3
forward 9
down 4
up 8
up 3
down 5
forward 4
down 4
down 6
down 8
forward 2
down 7
forward 9
up 7
down 3
forward 3
forward 2
down 4
down 4
forward 2
forward 5
forward 9
down 9
down 2
forward 1
up 4
forward 9
forward 9
up 4
down 2
up 1
up 7
forward 7
forward 6
forward 1
forward 8
forward 2
forward 4
up 4
down 5
up 8
down 4
forward 4
forward 2
forward 2
down 4
up 3
down 7
down 5
forward 7
forward 9
down 8
up 1
forward 5
up 9
down 1
up 5
down 4
down 2
down 5
down 7
down 3
down 9
forward 8
up 3
forward 4
down 6
down 4
up 5
down 3
up 4
forward 8
forward 2
down 1
down 6
down 9
down 8
forward 2
down 6
down 2
up 7
down 6
down 4
up 2
forward 7
down 4
forward 4
up 8
down 1
down 6
forward 6
down 5
down 7
up 9
forward 2
up 1
down 1
up 8
forward 6
forward 7
up 1
down 5
down 3
up 9
down 2
forward 5
forward 1
up 6
up 4
forward 6
down 8
forward 7
forward 9
forward 2
forward 3
forward 9
forward 4
down 9
down 4
forward 9
forward 4
forward 9
forward 2
forward 1
down 3
forward 5
forward 2
forward 7
down 7
down 4
up 7
forward 3
forward 8
forward 4
down 4
forward 9
forward 2
forward 9
down 5
forward 2
down 5
forward 2
down 8
up 8
down 7
forward 2
forward 5
down 5
forward 8
up 9
forward 7
forward 5
forward 3
down 4
down 2
forward 4
down 4
forward 6
down 4
forward 6
forward 2
down 9
forward 7
down 5
up 5
down 9
down 5
down 3
up 4
down 6
down 2
forward 5
down 4
down 1
down 1
up 7
forward 9
forward 6
down 2
down 8
up 7
up 2
forward 9
down 8
up 6
down 8
down 6
down 2
down 9
forward 5
down 1
forward 6
forward 2
forward 4
down 7
down 4
down 8
forward 8
up 5
up 8
up 2
up 2
up 2
down 5
forward 6
down 8
down 2
up 5
up 2
forward 7
forward 4
up 9
forward 7
up 7
down 9
forward 8
down 5
forward 4
down 6
forward 5
up 2
down 9
down 3
down 2
down 3
forward 2
forward 4
forward 9
up 6
forward 8
down 9
forward 1
forward 1
forward 1
forward 4
forward 3
up 5
forward 2
down 8
forward 6
down 3
down 9
up 6
forward 7
up 3
down 1
forward 1
forward 1
forward 2
forward 5
down 9
forward 1
forward 1
down 2
forward 2
forward 4
up 8
down 7
forward 1
up 3
forward 7
down 6
forward 5
up 4
up 5
forward 4
down 7
down 7
down 9
down 3
down 1
down 1
down 9
down 7
forward 4
up 3
down 8
down 3
forward 7
down 1
down 8
forward 5
down 5
down 1
forward 5
forward 6
down 3
down 8
down 1
forward 4
up 5
forward 5
down 2
down 2
forward 5
down 5
forward 5
forward 1
down 2
down 3
down 3
down 9
down 6
forward 6
forward 9
forward 9
forward 6
up 2
up 4
forward 7
up 4
down 4
forward 4
down 4
up 2
forward 5
up 9
down 8
down 8
down 1
down 9
forward 9
down 4
up 2
up 3
down 2
forward 6
up 4
up 7
down 3
up 2
forward 6
down 7
down 3
down 6
down 6
forward 3
forward 7
up 8
down 6
forward 2
down 3
forward 5
down 3
forward 6
up 4
forward 1
forward 9
up 9
forward 3
forward 5
down 2
down 4
forward 4
forward 8
down 4
forward 2
forward 1
forward 9
forward 6
forward 1
down 7
up 4
down 8
forward 3
down 4
forward 1
forward 1
up 7
up 7
down 1
forward 5
down 1
forward 1
forward 2
forward 7
forward 2
down 7
up 9
up 2
forward 2
forward 8
down 7
down 6
down 8
up 8
down 9
up 5
forward 3
down 6
forward 7
up 7
down 1
forward 6
forward 2
forward 6
forward 6
down 7
down 2
up 7
down 6
down 8
up 2
down 9
up 6
up 8
up 1
forward 2
down 5
down 3
down 2
down 3
forward 9
forward 4
down 1
down 1
up 4
down 7
up 1
down 9
up 6
down 2
down 6
forward 3
down 8
down 5
down 6
down 2
down 4
forward 2
forward 2
down 9
up 5
forward 7
up 8
down 8
down 5
down 3
down 1
forward 9
forward 1
up 1
down 8
forward 8
down 5
forward 5
down 2
forward 2
forward 1
forward 1
forward 7
forward 8
forward 9
forward 2
down 7
forward 4
forward 8
forward 7
up 9
down 5
down 6
down 4
up 9
up 8
down 1
forward 3
down 6
down 8
forward 1
forward 9
down 5
down 9
down 5
forward 5
forward 3
forward 9
down 2
down 4
forward 9
forward 4
down 5
forward 9
down 5
up 2
down 5
forward 8
down 9
up 6
down 6
up 5
down 1
down 9
down 4
down 7
down 3
up 4
forward 4
forward 3
down 4
forward 9
forward 4
up 1
forward 8
up 8
down 1
forward 3
forward 4
forward 3
forward 4
forward 6
down 6
forward 3
down 4
forward 2
down 5
down 4
forward 8
up 8
down 6
down 7
forward 6
forward 3
down 6
down 1
up 7
down 8
down 7
down 6
down 5
down 1
down 7
up 9
forward 5
forward 8
down 6
down 1
down 4
down 5
forward 5
down 5
down 1
down 7
up 1
down 1
forward 8
forward 3
down 9
up 7
down 4
down 5
forward 5
forward 8
forward 2
down 6
down 1
up 5
forward 3
down 9
forward 7
forward 6
down 6
forward 7
down 6
down 5
down 2
up 5
down 3
forward 4
forward 2
down 4
down 9
down 4
forward 2
forward 7
forward 8
down 8
forward 4
forward 5
down 9
forward 7
down 3
down 9
down 9
up 7
down 8
up 9
forward 2
up 4
forward 5
down 4
up 2
up 9
forward 4
forward 7
down 8
up 2
forward 5
down 1
down 2
forward 8
down 4
forward 2
forward 5
forward 3
down 7
forward 6
up 8
down 2
forward 2
down 8
forward 7
up 7
down 9
forward 4
up 8
down 8
forward 3
forward 2
up 8
forward 8
down 8
down 8
forward 4
down 6
down 6
forward 7
forward 2
forward 1
up 3
forward 7
up 4
up 5
down 7
down 1
forward 5
down 2
down 3
down 1
down 5
forward 6
down 7
forward 6
up 4
down 7
forward 6
up 7
up 5
up 3
forward 5
forward 4
forward 6
forward 8
up 5
forward 7
down 5
forward 6
down 8
down 3
down 4
forward 6
up 7
down 2
forward 8
forward 5
forward 1
down 2
down 6
down 9
up 7
up 7
forward 6
forward 6
up 3
down 5
forward 7
forward 7
down 5
forward 3
up 6
forward 6
forward 1
down 8
forward 8
down 8
up 8
forward 3
down 4
up 7
forward 2
down 5
forward 2
up 3
forward 2
forward 1
up 4
up 5
forward 5
down 9
forward 9
up 1
up 1
down 5
down 7
forward 6
forward 1
up 2
down 9
down 7
down 5
forward 6
up 7
down 4
forward 6
forward 7
forward 2
down 6
down 8
forward 5
forward 7
down 9
down 8
up 9
forward 7
down 3
down 6
down 1
down 9
down 3
forward 6
down 3
down 2
up 5
forward 6
forward 9
forward 5
down 4
forward 1
forward 4
up 2
forward 3
up 1
down 4
forward 8
forward 2
forward 1
down 1
forward 7
down 2
forward 1
forward 2
forward 8
forward 5
down 2
up 6
up 5
up 9
down 6
forward 8
up 6
forward 9
forward 2
down 7
down 8
up 4
down 1
up 5
up 5
forward 7
down 7
forward 3
down 7
down 1
forward 6
forward 7
down 2
forward 4
down 5
forward 1
down 6
down 4
forward 4
up 2
down 8
up 2
up 2
forward 1
down 3
forward 1
forward 4
up 4
up 4
down 7
down 2
forward 2
forward 1
up 2
forward 2
forward 8
down 1
down 8
forward 3
down 6
up 1
forward 7
forward 8
forward 4
down 5
down 6
down 3
down 6
down 4
forward 8
up 4
up 4
down 1
down 8
down 3
down 6
forward 2
down 6
forward 3
up 1
forward 9
up 6
up 6
forward 9
forward 5
up 8
down 2
down 9
forward 4
down 5
forward 8
down 5
up 6
forward 1
down 6
up 9
down 3
forward 7
down 4
down 3
down 7
down 6
forward 1
forward 8
forward 2
forward 2
forward 5
forward 5
forward 7
forward 2
down 2
forward 2
up 9
down 8
down 3
forward 2
forward 1
down 4
down 9
forward 5
forward 4
forward 5
down 1
up 4
down 2
up 7
down 1
down 5
forward 5
up 6
down 6
forward 8
down 3
up 8
up 2
down 9
forward 2
down 8
forward 4
forward 3
forward 8
down 6
forward 2
down 5
forward 7
down 4
down 2
down 7
up 2
forward 8
up 8
down 2
forward 9
down 4
down 8
down 8
down 6
forward 6
down 3
forward 1
forward 5
down 3
down 3
up 8
down 1
down 1
forward 1
up 4
forward 9
forward 1
up 4
up 2
forward 4
forward 1
forward 6
forward 1
forward 1
forward 6
up 9
forward 9
up 4
forward 2
forward 9
forward 2
forward 4
forward 9
down 6
forward 5
forward 9
down 6
down 8
forward 5
down 4
forward 6
forward 4
down 4
down 1
forward 8
up 7
down 1
down 2
up 7
forward 6
down 5
up 6
down 7
down 8
down 2
up 5
forward 1
up 5
down 1
down 6
down 2
down 3
forward 8
down 5
forward 6
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
