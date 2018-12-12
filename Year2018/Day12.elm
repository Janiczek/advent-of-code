module Year2018.Day12 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)
import List.Extra



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    { state : String
    , leftmostAlive : Int
    , rules : Dict String Bool
    }


type alias Input2 =
    Input1


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    let
        lines =
            String.lines string

        initial =
            Advent.unsafeMaybe (List.head lines)

        rules =
            List.drop 2 lines
    in
    { state = parseInitial initial
    , leftmostAlive = 0
    , rules = parseRules rules
    }


parseInitial : String -> String
parseInitial string =
    case String.words string of
        [ _, _, state ] ->
            state

        _ ->
            Debug.todo "wrong input 1"


parseRules : List String -> Dict String Bool
parseRules lines =
    lines
        |> List.map parseRule
        |> Dict.fromList


parseRule : String -> ( String, Bool )
parseRule string =
    case String.split " => " string of
        [ state, result ] ->
            ( state, result == "#" )

        _ ->
            Debug.todo "wrong input 2"


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 { state, leftmostAlive, rules } =
    ( state, leftmostAlive )
        |> step 20 0 rules
        |> show 20
        |> indexesOfAlive
        |> List.sum


indexesOfAlive : ( String, Int ) -> List Int
indexesOfAlive ( state, leftmostAlive ) =
    state
        |> String.toList
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( i, c ) ->
                if c == '#' then
                    Just (i + leftmostAlive)

                else
                    Nothing
            )


step : Int -> Int -> Dict String Bool -> ( String, Int ) -> ( String, Int )
step endingStep stepNumber rules ( state, leftmostAlive ) =
    if stepNumber == endingStep then
        ( state, leftmostAlive )

    else
        let
            _ =
                show stepNumber ( state, leftmostAlive )
        in
        step endingStep (stepNumber + 1) rules (stepOnce rules ( state, leftmostAlive ))


stepOnce : Dict String Bool -> ( String, Int ) -> ( String, Int )
stepOnce rules ( state, leftmostAlive ) =
    ( state, leftmostAlive )
        |> Tuple.mapFirst pad
        |> Tuple.mapFirst group
        |> transformAll rules
        |> trim


pad : String -> String
pad state =
    "...." ++ state ++ "...."


group : String -> List (List Char)
group state =
    state
        |> String.toList
        |> List.Extra.groupsOfWithStep 5 1


show : Int -> ( String, Int ) -> ( String, Int )
show step_ ( state, leftmostAlive ) =
    let
        _ =
            Debug.log
                (String.fromInt step_
                    ++ ","
                    ++ String.fromInt leftmostAlive
                    ++ ","
                    ++ String.fromInt
                        (( state, leftmostAlive )
                            |> indexesOfAlive
                            |> List.sum
                        )
                )
                state
    in
    ( state, leftmostAlive )


transformAll : Dict String Bool -> ( List (List Char), Int ) -> ( String, Int )
transformAll rules ( groups, leftmostAlive ) =
    let
        newState =
            groups
                |> List.map (transform rules)
                |> String.fromList
    in
    ( newState, leftmostAlive - 2 )


transform : Dict String Bool -> List Char -> Char
transform rules group_ =
    let
        stringGroup =
            String.fromList group_

        newIsAlive =
            Dict.get stringGroup rules
                |> Maybe.withDefault False
    in
    if newIsAlive then
        '#'

    else
        '.'


trim : ( String, Int ) -> ( String, Int )
trim ( state, leftmostAlive ) =
    if String.startsWith "." state then
        trim ( String.dropLeft 1 state, leftmostAlive + 1 )

    else if String.endsWith "." state then
        trim ( String.dropRight 1 state, leftmostAlive )

    else
        ( state, leftmostAlive )


compute2 : Input2 -> Output2
compute2 { state, leftmostAlive, rules } =
    let
        wantedStep : Int
        wantedStep =
            50000000000

        -- at step 124 it starts to cycle,
        -- from pot 53 onwards there's a constant pattern of pots
        -- at step 125 the same pattern starts at pot 54, etc.
        -- so, the offset is 71
        offset : Int
        offset =
            71

        pattern : String
        pattern =
            "##.....##...##...##...##...##...##..#...##...##...##..........##...##...##...##...##..#...##...##.....##...##...##...##...##...##...##...##......##...##...##...##..#...##"
    in
    ( pattern, wantedStep - offset )
        |> indexesOfAlive
        |> List.sum



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
initial state: #..######..#....#####..###.##..#######.####...####.##..#....#.##.....########.#...#.####........#.#.

#...# => .
##.## => .
###.. => .
.#### => .
#.#.# => .
##..# => #
..#.# => #
.##.. => #
##... => #
#..## => #
#..#. => .
.###. => #
#.##. => .
..### => .
.##.# => #
....# => .
##### => .
#.### => .
.#..# => .
#.... => .
...## => .
.#.## => .
##.#. => #
#.#.. => #
..... => .
.#... => #
...#. => #
..#.. => .
..##. => .
###.# => .
####. => .
.#.#. => .
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
