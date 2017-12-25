module Year2017.Day25 exposing (..)

import Advent exposing (Test)
import Dict exposing (Dict)
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


type alias Tape =
    Dict Int Int


get : Int -> Tape -> Int
get position tape =
    tape
        |> Dict.get position
        |> Maybe.withDefault 0


type alias State =
    String


type alias StepsToChecksum =
    Int


type alias Decision =
    { valueToWrite : Int
    , moveTo : Direction
    , nextState : State
    }


type Direction
    = Left
    | Right


type alias Rule =
    { if0 : Decision
    , if1 : Decision
    }


type alias Input =
    { state : State
    , stepsToChecksum : StepsToChecksum
    , rules : Dict State Rule
    , position : Int
    }


type alias Output =
    Int


compute1 : Input -> Output
compute1 input =
    run emptyTape input


emptyTape : Tape
emptyTape =
    Dict.empty


run : Tape -> Input -> Int
run tape { state, stepsToChecksum, rules, position } =
    if stepsToChecksum == 0 then
        countOnes tape
    else
        let
            _ =
                if stepsToChecksum % 100000 == 0 then
                    Debug.log (toString stepsToChecksum) ( state, position )
                else
                    ( state, position )

            currentValue : Int
            currentValue =
                tape |> get position

            rule : Rule
            rule =
                rules |> Dict.get state |> Advent.unsafeMaybe

            decision : Decision
            decision =
                case currentValue of
                    0 ->
                        rule.if0

                    1 ->
                        rule.if1

                    _ ->
                        Debug.crash "value other than 0/1 ????"

            newPosition =
                move decision.moveTo position

            newState =
                decision.nextState

            newTape =
                tape |> Dict.insert position decision.valueToWrite
        in
            run newTape
                { state = newState
                , stepsToChecksum = stepsToChecksum - 1
                , rules = rules
                , position = newPosition
                }


move : Direction -> Int -> Int
move direction position =
    case direction of
        Left ->
            position - 1

        Right ->
            position + 1


countOnes : Tape -> Int
countOnes tape =
    tape
        |> Dict.filter (\k v -> v == 1)
        |> Dict.size


compute2 : Input -> Output
compute2 input =
    -1


tests1 : List (Test Input Output)
tests1 =
    [ Test "example"
        """Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A."""
        { state = "A"
        , stepsToChecksum = 6
        , position = 0
        , rules =
            [ ( "A"
              , { if0 =
                    { valueToWrite = 1
                    , moveTo = Right
                    , nextState = "B"
                    }
                , if1 =
                    { valueToWrite = 0
                    , moveTo = Left
                    , nextState = "B"
                    }
                }
              )
            , ( "B"
              , { if0 =
                    { valueToWrite = 1
                    , moveTo = Left
                    , nextState = "A"
                    }
                , if1 =
                    { valueToWrite = 1
                    , moveTo = Right
                    , nextState = "A"
                    }
                }
              )
            ]
                |> Dict.fromList
        }
        3
    ]


tests2 : List (Test Input Output)
tests2 =
    []


parse : String -> Input
parse input =
    case String.lines input of
        startStateLine :: checksumLine :: rulesLines ->
            { state = parseState startStateLine
            , stepsToChecksum = parseChecksum checksumLine
            , rules = parseRules rulesLines
            , position = 0
            }

        _ ->
            Debug.crash "wrong input!"


parseState : String -> State
parseState string =
    string
        |> String.dropRight 1
        |> String.words
        |> List.reverse
        |> List.head
        |> Advent.unsafeMaybe


parseChecksum : String -> Int
parseChecksum string =
    string
        |> String.words
        |> List.reverse
        |> List.drop 1
        |> List.head
        |> Maybe.map Advent.toInt
        |> Advent.unsafeMaybe


parseRules : List String -> Dict State Rule
parseRules rulesLines =
    rulesLines
        -- every rule ("in state x .....") has 10 lines
        -- including the empty line above it
        |> List.Extra.groupsOf 10
        |> List.map parseRule
        |> Dict.fromList


parseRule : List String -> ( State, Rule )
parseRule ruleLines =
    --{ valueToWrite : Int
    --, moveTo : Direction
    --, nextState : State
    --}
    case ruleLines of
        [ _, stateLine, _, zeroValueToWrite, zeroMoveTo, zeroNextState, _, oneValueToWrite, oneMoveTo, oneNextState ] ->
            ( parseState stateLine
            , { if0 =
                    { valueToWrite = parseValueToWrite zeroValueToWrite
                    , moveTo = parseMoveTo zeroMoveTo
                    , nextState = parseState zeroNextState
                    }
              , if1 =
                    { valueToWrite = parseValueToWrite oneValueToWrite
                    , moveTo = parseMoveTo oneMoveTo
                    , nextState = parseState oneNextState
                    }
              }
            )

        _ ->
            Debug.crash "wrong input??"


parseMoveTo : String -> Direction
parseMoveTo string =
    string
        |> String.dropRight 1
        |> String.words
        |> List.reverse
        |> List.head
        |> Maybe.map parseDirection
        |> Advent.unsafeMaybe


parseDirection : String -> Direction
parseDirection string =
    case string of
        "left" ->
            Left

        "right" ->
            Right

        _ ->
            Debug.crash "wrong input!"


parseValueToWrite : String -> Int
parseValueToWrite string =
    string
        |> String.dropRight 1
        |> String.words
        |> List.reverse
        |> List.head
        |> Maybe.map Advent.toInt
        |> Advent.unsafeMaybe


input : String
input =
    """Begin in state A.
Perform a diagnostic checksum after 12523873 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state E.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state C.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state F.

In state C:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state D.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the right.
    - Continue with state B.

In state D:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state E.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state C.

In state E:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the right.
    - Continue with state D.

In state F:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state C."""
