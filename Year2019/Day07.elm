module Year2019.Day07 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import List.Extra
import Year2019.Intcode as Intcode exposing (Mask(..), Memory, Op(..), Parameter)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    Memory


type alias Input2 =
    Memory


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    Intcode.parse string


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


type alias LogEntry =
    Int


type alias Log =
    List LogEntry


supportedOps : List ( Int, Intcode.Op Op )
supportedOps =
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
            (\dest -> SetInputAt { dest = dest })
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


parseOpcode : Int -> Memory -> Maybe Op
parseOpcode =
    Intcode.parseWith supportedOps


process :
    List Int
    -> Op
    -> Int
    -> Memory
    ->
        { position : Maybe Int
        , logEntry : Maybe LogEntry
        , memory : Memory
        , inputs : List Int
        }
process inputs op position mem =
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
            { position = Just (position + 4)
            , logEntry = Nothing
            , memory = newMem
            , inputs = inputs
            }

        Mult { addr0, addr1, dest } ->
            let
                num0 =
                    Intcode.getParam addr0 mem

                num1 =
                    Intcode.getParam addr1 mem

                newMem =
                    Intcode.setParam dest (num0 * num1) mem
            in
            { position = Just (position + 4)
            , logEntry = Nothing
            , memory = newMem
            , inputs = inputs
            }

        SetInputAt { dest } ->
            let
                ( input, restOfInputs ) =
                    List.Extra.uncons inputs
                        |> Advent.unsafeMaybe

                newMem =
                    Intcode.setParam dest input mem
            in
            { position = Just (position + 2)
            , logEntry = Nothing
            , memory = newMem
            , inputs = restOfInputs
            }

        Print { addr } ->
            let
                logEntry =
                    Just <| Intcode.getParam addr mem
            in
            { position = Just (position + 2)
            , logEntry = logEntry
            , memory = mem
            , inputs = inputs
            }

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
            { position = Just newPosition
            , logEntry = Nothing
            , memory = mem
            , inputs = inputs
            }

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
            { position = Just newPosition
            , logEntry = Nothing
            , memory = mem
            , inputs = inputs
            }

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
            { position = Just (position + 4)
            , logEntry = Nothing
            , memory = newMem
            , inputs = inputs
            }

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
            { position = Just (position + 4)
            , logEntry = Nothing
            , memory = newMem
            , inputs = inputs
            }

        Halt ->
            { position = Nothing
            , logEntry = Nothing
            , memory = mem
            , inputs = inputs
            }


type Op
    = Add { addr0 : Parameter, addr1 : Parameter, dest : Parameter }
    | Mult { addr0 : Parameter, addr1 : Parameter, dest : Parameter }
    | SetInputAt { dest : Parameter }
    | Print { addr : Parameter }
    | JumpIfTrue { test : Parameter, jumpTo : Parameter }
    | JumpIfFalse { test : Parameter, jumpTo : Parameter }
    | LessThan { left : Parameter, right : Parameter, dest : Parameter }
    | Equals { left : Parameter, right : Parameter, dest : Parameter }
    | Halt


compute1 : Input1 -> Output1
compute1 mem =
    List.Extra.permutations (List.range 0 4)
        |> List.map (permutationToRecord >> tryPermutation1 mem)
        |> List.maximum
        |> Advent.unsafeMaybe


type alias Permutation =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
    , e : Int
    }


permutationToRecord : List Int -> Permutation
permutationToRecord list =
    case list of
        [ a, b, c, d, e ] ->
            { a = a
            , b = b
            , c = c
            , d = d
            , e = e
            }

        _ ->
            Debug.todo "perm to record?"


tryPermutation1 : Memory -> Permutation -> Int
tryPermutation1 mem { a, b, c, d, e } =
    0
        |> runProgram1 mem a
        |> runProgram1 mem b
        |> runProgram1 mem c
        |> runProgram1 mem d
        |> runProgram1 mem e


runProgram1 : Memory -> Int -> Int -> Int
runProgram1 mem phase previousOutput =
    case go1 [ phase, previousOutput ] [] 0 mem of
        [ output ] ->
            output

        somethingElse ->
            Debug.todo <| "what? " ++ Debug.toString somethingElse


go1 : List Int -> Log -> Int -> Memory -> Log
go1 inputs log position mem =
    case Intcode.step parseOpcode (process inputs) position mem of
        Err err ->
            let
                _ =
                    Debug.log "Error" err
            in
            Debug.todo "Crashed when stepping an Intcode program"

        Ok r ->
            case r.position of
                Nothing ->
                    -- we've HALTed
                    log

                Just newPosition ->
                    let
                        newLog =
                            r.logEntry
                                |> Maybe.map (\logEntry -> logEntry :: log)
                                |> Maybe.withDefault log
                    in
                    go1 r.inputs newLog newPosition r.memory


compute2 : Input2 -> Output2
compute2 mem =
    List.Extra.permutations (List.range 5 9)
        |> List.map (permutationToRecord >> tryPermutation2 mem)
        |> List.maximum
        |> Advent.unsafeMaybe


tryPermutation2 : Memory -> Permutation -> Int
tryPermutation2 mem permutation =
    runTheLoop permutation ( 0, mem )


type alias Amp =
    { mem : Memory
    , position : Maybe Int
    }


runTheLoop : Permutation -> ( Int, Memory ) -> Int
runTheLoop ({ a, b, c, d, e } as permutation) ( input, mem ) =
    let
        ( outputA, ampA ) =
            runProgram2 "A" [ a, input ] 0 mem

        ( outputB, ampB ) =
            runProgram2 "B" [ b, outputA ] 0 mem

        ( outputC, ampC ) =
            runProgram2 "C" [ c, outputB ] 0 mem

        ( outputD, ampD ) =
            runProgram2 "D" [ d, outputC ] 0 mem

        ( outputE, ampE ) =
            runProgram2 "E" [ e, outputD ] 0 mem
    in
    runTheLoopWithoutPhase
        { outputE = outputE
        , ampA = ampA
        , ampB = ampB
        , ampC = ampC
        , ampD = ampD
        , ampE = ampE
        }


runTheLoopWithoutPhase :
    { outputE : Int
    , ampA : Amp
    , ampB : Amp
    , ampC : Amp
    , ampD : Amp
    , ampE : Amp
    }
    -> Int
runTheLoopWithoutPhase { outputE, ampA, ampB, ampC, ampD, ampE } =
    case List.filterMap identity [ ampA.position, ampB.position, ampC.position, ampD.position, ampE.position ] of
        [ posA, posB, posC, posD, posE ] ->
            let
                ( outputA, newAmpA ) =
                    runProgram2 "A" [ outputE ] posA ampA.mem

                ( outputB, newAmpB ) =
                    runProgram2 "B" [ outputA ] posB ampB.mem

                ( outputC, newAmpC ) =
                    runProgram2 "C" [ outputB ] posC ampC.mem

                ( outputD, newAmpD ) =
                    runProgram2 "D" [ outputC ] posD ampD.mem

                ( newOutputE, newAmpE ) =
                    runProgram2 "E" [ outputD ] posE ampE.mem
            in
            runTheLoopWithoutPhase
                { outputE = newOutputE
                , ampA = newAmpA
                , ampB = newAmpB
                , ampC = newAmpC
                , ampD = newAmpD
                , ampE = newAmpE
                }

        _ ->
            outputE


runProgram2 : String -> List Int -> Int -> Memory -> ( Int, Amp )
runProgram2 id inputs position mem =
    case go2 id inputs [] position mem of
        Halted r ->
            case inputs of
                [ output ] ->
                    ( output
                    , { mem = r.mem
                      , position = Nothing
                      }
                    )

                somethingElse ->
                    Debug.todo <| "halted without one output? " ++ Debug.toString somethingElse

        Emitted r ->
            ( r.output
            , { mem = r.mem
              , position = Just r.position
              }
            )


type State
    = Halted
        { log : Log
        , mem : Memory
        }
    | Emitted
        { output : Int
        , mem : Memory
        , position : Int
        }


go2 : String -> List Int -> Log -> Int -> Memory -> State
go2 id inputs log position mem =
    case Intcode.step parseOpcode (process inputs) position mem of
        Err err ->
            let
                _ =
                    Debug.log "Error" err
            in
            Debug.todo "Crashed when stepping an Intcode program"

        Ok r ->
            case r.position of
                Nothing ->
                    Halted
                        { log = log
                        , mem = r.memory
                        }

                Just newPosition ->
                    case r.logEntry of
                        Just logEntry ->
                            Emitted
                                { output = logEntry
                                , mem = r.memory
                                , position = newPosition
                                }

                        Nothing ->
                            go2 id r.inputs log newPosition r.memory



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
    [ Test "ex"
        "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
        Nothing
        139629729
    ]



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
3,8,1001,8,10,8,105,1,0,0,21,34,51,68,89,98,179,260,341,422,99999,3,9,1001,9,4,9,102,4,9,9,4,9,99,3,9,1002,9,5,9,1001,9,2,9,1002,9,2,9,4,9,99,3,9,1001,9,3,9,102,3,9,9,101,4,9,9,4,9,99,3,9,102,2,9,9,101,2,9,9,1002,9,5,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99
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
