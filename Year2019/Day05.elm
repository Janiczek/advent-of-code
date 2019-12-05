module Year2019.Day05 exposing
    ( Input1
    , Input2
    , Output1
    , Output2
    , compute1
    , compute2
    , input_
    , main
    , parse1
    , parse2
    , supportedOps
    , tests1
    , tests2
    )

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Array
import Year2019.Intcode as Intcode
    exposing
        ( Mask(..)
        , Memory
        , Op(..)
        , Parameter
        )



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    ( Int, Memory )


type alias Input2 =
    ( Int, Memory )


type alias Output1 =
    Log


type alias Output2 =
    Log



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    case String.split "::" string of
        [ input, program ] ->
            ( Advent.unsafeToInt input, Intcode.parse program )

        _ ->
            Debug.todo "input doesn't contain the input"


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


type alias LogEntry =
    Int


type alias Log =
    List LogEntry


process : Op -> Int -> Memory -> ( Maybe Int, Maybe LogEntry, Memory )
process op position mem =
    case op of
        Add { addr0, addr1, dest } ->
            let
                num0 =
                    Intcode.getParam addr0 mem

                num1 =
                    Intcode.getParam addr1 mem

                newMem =
                    Intcode.setParam dest (num0 + num1) mem
            in
            ( Just (position + 4), Nothing, newMem )

        Mult { addr0, addr1, dest } ->
            let
                num0 =
                    Intcode.getParam addr0 mem

                num1 =
                    Intcode.getParam addr1 mem

                newMem =
                    Intcode.setParam dest (num0 * num1) mem
            in
            ( Just (position + 4), Nothing, newMem )

        SetInputAt { input, dest } ->
            let
                newMem =
                    Intcode.setParam dest input mem
            in
            ( Just (position + 2), Nothing, newMem )

        Print { addr } ->
            let
                logEntry =
                    Just <| Intcode.getParam addr mem
            in
            ( Just (position + 2), logEntry, mem )

        JumpIfTrue { test, jumpTo } ->
            let
                testValue =
                    Intcode.getParam test mem

                newPosition =
                    if testValue /= 0 then
                        Intcode.getParam jumpTo mem

                    else
                        position + 3
            in
            ( Just newPosition, Nothing, mem )

        JumpIfFalse { test, jumpTo } ->
            let
                testValue =
                    Intcode.getParam test mem

                newPosition =
                    if testValue == 0 then
                        Intcode.getParam jumpTo mem

                    else
                        position + 3
            in
            ( Just newPosition, Nothing, mem )

        LessThan { left, right, dest } ->
            let
                leftValue =
                    Intcode.getParam left mem

                rightValue =
                    Intcode.getParam right mem

                result =
                    if leftValue < rightValue then
                        1

                    else
                        0

                newMem =
                    Intcode.setParam dest result mem
            in
            ( Just (position + 4), Nothing, newMem )

        Equals { left, right, dest } ->
            let
                leftValue =
                    Intcode.getParam left mem

                rightValue =
                    Intcode.getParam right mem

                result =
                    if leftValue == rightValue then
                        1

                    else
                        0

                newMem =
                    Intcode.setParam dest result mem
            in
            ( Just (position + 4), Nothing, newMem )

        Halt ->
            ( Nothing, Nothing, mem )


parseOpcode1 : Int -> Int -> Memory -> Maybe Op
parseOpcode1 input =
    Intcode.parseWith
        [ ( 1
          , Op3
                ( DontCare, DontCare, WantPosition )
                (\addr0 addr1 dest ->
                    Add
                        { addr0 = addr0
                        , addr1 = addr1
                        , dest = dest
                        }
                )
          )
        , ( 2
          , Op3
                ( DontCare, DontCare, WantPosition )
                (\addr0 addr1 dest ->
                    Mult
                        { addr0 = addr0
                        , addr1 = addr1
                        , dest = dest
                        }
                )
          )
        , ( 3
          , Op1 WantPosition
                (\dest ->
                    SetInputAt
                        { input = input
                        , dest = dest
                        }
                )
          )
        , ( 4, Op1 DontCare (\addr -> Print { addr = addr }) )
        , ( 99, Op0 Halt )
        ]


parseOpcode2 : Int -> Int -> Memory -> Maybe Op
parseOpcode2 input =
    Intcode.parseWith (supportedOps input)


supportedOps : Int -> List ( Int, Intcode.Op Op )
supportedOps input =
    [ ( 1
      , Op3
            ( DontCare, DontCare, WantPosition )
            (\addr0 addr1 dest ->
                Add
                    { addr0 = addr0
                    , addr1 = addr1
                    , dest = dest
                    }
            )
      )
    , ( 2
      , Op3
            ( DontCare, DontCare, WantPosition )
            (\addr0 addr1 dest ->
                Mult
                    { addr0 = addr0
                    , addr1 = addr1
                    , dest = dest
                    }
            )
      )
    , ( 3
      , Op1 WantPosition
            (\dest ->
                SetInputAt
                    { input = input
                    , dest = dest
                    }
            )
      )
    , ( 4, Op1 DontCare (\addr -> Print { addr = addr }) )
    , ( 5
      , Op2 ( DontCare, DontCare )
            (\test jumpTo ->
                JumpIfTrue
                    { test = test
                    , jumpTo = jumpTo
                    }
            )
      )
    , ( 6
      , Op2 ( DontCare, DontCare )
            (\test jumpTo ->
                JumpIfFalse
                    { test = test
                    , jumpTo = jumpTo
                    }
            )
      )
    , ( 7
      , Op3 ( DontCare, DontCare, WantPosition )
            (\left right dest ->
                LessThan
                    { left = left
                    , right = right
                    , dest = dest
                    }
            )
      )
    , ( 8
      , Op3 ( DontCare, DontCare, WantPosition )
            (\left right dest ->
                Equals
                    { left = left
                    , right = right
                    , dest = dest
                    }
            )
      )
    , ( 99, Op0 Halt )
    ]


type Op
    = Add { addr0 : Parameter, addr1 : Parameter, dest : Parameter }
    | Mult { addr0 : Parameter, addr1 : Parameter, dest : Parameter }
    | SetInputAt { input : Int, dest : Parameter }
    | Print { addr : Parameter }
    | JumpIfTrue { test : Parameter, jumpTo : Parameter }
    | JumpIfFalse { test : Parameter, jumpTo : Parameter }
    | LessThan { left : Parameter, right : Parameter, dest : Parameter }
    | Equals { left : Parameter, right : Parameter, dest : Parameter }
    | Halt


go1 : Int -> Log -> Int -> Memory -> ( Memory, Log )
go1 _ log position mem =
    let
        -- ignore the 5 from part 2
        input =
            1
    in
    case Intcode.step (parseOpcode1 input) process position mem of
        Err err ->
            let
                _ =
                    Debug.log "Error" err
            in
            Debug.todo "Crashed when stepping an Intcode program"

        Ok ( Nothing, _, newMem ) ->
            -- we've HALTed
            ( newMem, List.reverse log )

        Ok ( Just newPosition, maybeLogEntry, newMem ) ->
            let
                newLog =
                    maybeLogEntry
                        |> Maybe.map (\logEntry -> logEntry :: log)
                        |> Maybe.withDefault log
            in
            go1 input newLog newPosition newMem


go2 : Int -> Log -> Int -> Memory -> ( Memory, Log )
go2 input log position mem =
    case Intcode.step (parseOpcode2 input) process position mem of
        Err err ->
            let
                _ =
                    Debug.log "Error" err
            in
            Debug.todo "Crashed when stepping an Intcode program"

        Ok ( Nothing, _, newMem ) ->
            -- we've HALTed
            ( newMem, List.reverse log )

        Ok ( Just newPosition, maybeLogEntry, newMem ) ->
            let
                newLog =
                    maybeLogEntry
                        |> Maybe.map (\logEntry -> logEntry :: log)
                        |> Maybe.withDefault log
            in
            go2 input newLog newPosition newMem


compute1 : Input1 -> Output1
compute1 ( input, mem ) =
    go1 input [] 0 mem
        |> Tuple.second


compute2 : Input2 -> Output2
compute2 ( input, mem ) =
    go2 input [] 0 mem
        |> Tuple.second



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
    [ Test "ex [equal 8 position] 1" "0::3,9,8,9,10,9,4,9,99,-1,8" Nothing [ 0 ]
    , Test "ex [equal 8 position] 2" "7::3,9,8,9,10,9,4,9,99,-1,8" Nothing [ 0 ]
    , Test "ex [equal 8 position] 3" "8::3,9,8,9,10,9,4,9,99,-1,8" Nothing [ 1 ]
    , Test "ex [equal 8 position] 4" "9::3,9,8,9,10,9,4,9,99,-1,8" Nothing [ 0 ]
    , Test "ex [less than 8 position] 1" "0::3,9,7,9,10,9,4,9,99,-1,8" Nothing [ 1 ]
    , Test "ex [less than 8 position] 2" "7::3,9,7,9,10,9,4,9,99,-1,8" Nothing [ 1 ]
    , Test "ex [less than 8 position] 3" "8::3,9,7,9,10,9,4,9,99,-1,8" Nothing [ 0 ]
    , Test "ex [less than 8 position] 4" "9::3,9,7,9,10,9,4,9,99,-1,8" Nothing [ 0 ]
    , Test "ex [equal 8 immediate] 1" "0::3,3,1108,-1,8,3,4,3,99" Nothing [ 0 ]
    , Test "ex [equal 8 immediate] 2" "7::3,3,1108,-1,8,3,4,3,99" Nothing [ 0 ]
    , Test "ex [equal 8 immediate] 3" "8::3,3,1108,-1,8,3,4,3,99" Nothing [ 1 ]
    , Test "ex [equal 8 immediate] 4" "9::3,3,1108,-1,8,3,4,3,99" Nothing [ 0 ]
    , Test "ex [less than 8 immediate] 1" "0::3,3,1107,-1,8,3,4,3,99" Nothing [ 1 ]
    , Test "ex [less than 8 immediate] 2" "7::3,3,1107,-1,8,3,4,3,99" Nothing [ 1 ]
    , Test "ex [less than 8 immediate] 3" "8::3,3,1107,-1,8,3,4,3,99" Nothing [ 0 ]
    , Test "ex [less than 8 immediate] 4" "9::3,3,1107,-1,8,3,4,3,99" Nothing [ 0 ]
    , Test "ex [jump position] 1" "0::3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" Nothing [ 0 ]
    , Test "ex [jump position] 2" "1::3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" Nothing [ 1 ]
    , Test "ex [jump immediate] 1" "0::3,3,1105,-1,9,1101,0,0,12,4,12,99,1" Nothing [ 0 ]
    , Test "ex [jump immediate] 2" "1::3,3,1105,-1,9,1101,0,0,12,4,12,99,1" Nothing [ 1 ]
    , Test "ex [big] 1" "7::3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" Nothing [ 999 ]
    , Test "ex [big] 2" "8::3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" Nothing [ 1000 ]
    , Test "ex [big] 3" "9::3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" Nothing [ 1001 ]
    ]



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
5::3,225,1,225,6,6,1100,1,238,225,104,0,1101,48,82,225,102,59,84,224,1001,224,-944,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,1101,92,58,224,101,-150,224,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1102,10,89,224,101,-890,224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,1101,29,16,225,101,23,110,224,1001,224,-95,224,4,224,102,8,223,223,1001,224,3,224,1,223,224,223,1102,75,72,225,1102,51,8,225,1102,26,16,225,1102,8,49,225,1001,122,64,224,1001,224,-113,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1102,55,72,225,1002,174,28,224,101,-896,224,224,4,224,1002,223,8,223,101,4,224,224,1,224,223,223,1102,57,32,225,2,113,117,224,101,-1326,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1,148,13,224,101,-120,224,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,677,226,224,102,2,223,223,1006,224,329,101,1,223,223,107,677,677,224,1002,223,2,223,1006,224,344,101,1,223,223,8,226,677,224,102,2,223,223,1006,224,359,101,1,223,223,107,226,226,224,102,2,223,223,1005,224,374,1001,223,1,223,1108,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,107,677,226,224,102,2,223,223,1006,224,404,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,419,1001,223,1,223,108,677,677,224,102,2,223,223,1005,224,434,1001,223,1,223,1008,677,226,224,1002,223,2,223,1006,224,449,1001,223,1,223,7,226,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1007,677,677,224,102,2,223,223,1005,224,479,1001,223,1,223,1007,226,226,224,1002,223,2,223,1005,224,494,1001,223,1,223,108,226,226,224,1002,223,2,223,1005,224,509,1001,223,1,223,1007,226,677,224,1002,223,2,223,1006,224,524,101,1,223,223,1107,677,677,224,102,2,223,223,1005,224,539,101,1,223,223,1107,677,226,224,102,2,223,223,1005,224,554,1001,223,1,223,108,677,226,224,1002,223,2,223,1006,224,569,1001,223,1,223,1108,226,677,224,1002,223,2,223,1006,224,584,101,1,223,223,8,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,614,101,1,223,223,7,677,677,224,1002,223,2,223,1006,224,629,101,1,223,223,1008,677,677,224,102,2,223,223,1005,224,644,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,659,101,1,223,223,1108,226,226,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226
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
