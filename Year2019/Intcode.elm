module Year2019.Intcode exposing
    ( Computer
    , Mask(..)
    , Op(..)
    , OutputError(..)
    , Stop(..)
    , ToOp(..)
    , addInput
    , getOutput
    , getOutputs
    , initWithMemory
    , map
    , step
    , stepUntilStopped
    , unwrap
    )

{- Intcode programs:

      2019-02
      2019-05 (position/immediate, new opcodes)
      2019-07
      2019-09 (relative base)
      2019-11
      2019-13
      2019-15
      2019-17

   TODO change from Result Stop Computer to

       Paused {...}
       Halted {...}
       WaitsForInput {...}
       ...


-}

import Advent
import Fifo exposing (Fifo)
import Year2019.Intcode.Memory as Memory exposing (Memory)
import Year2019.Intcode.Parameter as Parameter exposing (Parameter(..))


type alias Computer =
    { memory : Memory
    , relativeBase : Int
    , position : Int
    , inputs : Fifo Int
    , outputs : Fifo Int
    }


initWithMemory : Memory -> Computer
initWithMemory memory =
    { memory = memory
    , relativeBase = 0
    , position = 0
    , inputs = Fifo.empty
    , outputs = Fifo.empty
    }


addInput : Int -> Computer -> Computer
addInput input computer =
    { computer | inputs = Fifo.insert input computer.inputs }


addOutput : Int -> Computer -> Computer
addOutput output computer =
    { computer | outputs = Fifo.insert output computer.outputs }


setOutputs : Fifo Int -> Computer -> Computer
setOutputs outputs computer =
    { computer | outputs = outputs }


getOutput : Computer -> Result OutputError ( Int, Computer )
getOutput computer =
    case Fifo.remove computer.outputs of
        ( Nothing, _ ) ->
            Err NoOutputToGive

        ( Just output, newOutputs ) ->
            Ok
                ( output
                , computer
                    |> setOutputs newOutputs
                )


getOutputs : Result Stop Computer -> ( List Int, Result Stop Computer )
getOutputs result =
    ( result
        |> unwrap
        |> .outputs
        |> Fifo.toList
    , map
        (\computer -> { computer | outputs = Fifo.empty })
        result
    )


map : (Computer -> Computer) -> Result Stop Computer -> Result Stop Computer
map fn result =
    case result of
        Ok computer ->
            Ok (fn computer)

        Err (UnknownOpcode op computer) ->
            Err (UnknownOpcode op (fn computer))

        Err (Halted computer) ->
            Err (Halted (fn computer))

        Err (WaitsForInput computer) ->
            Err (WaitsForInput (fn computer))


unwrap : Result Stop Computer -> Computer
unwrap result =
    case result of
        Ok computer ->
            computer

        Err (UnknownOpcode _ computer) ->
            computer

        Err (Halted computer) ->
            computer

        Err (WaitsForInput computer) ->
            computer


jumpTo : Int -> Computer -> Computer
jumpTo position computer =
    { computer | position = position }


jumpBy : Int -> Computer -> Computer
jumpBy amount computer =
    { computer | position = computer.position + amount }


setMemory : Memory -> Computer -> Computer
setMemory memory computer =
    { computer | memory = memory }


setInputs : Fifo Int -> Computer -> Computer
setInputs inputs computer =
    { computer | inputs = inputs }


setRelativeBase : Int -> Computer -> Computer
setRelativeBase relativeBase computer =
    { computer | relativeBase = relativeBase }


param : Mask -> Int -> Int -> Parameter
param mask mode rawParam =
    case ( mode, mask ) of
        ( 0, NoImmediate ) ->
            Position rawParam

        ( 1, NoImmediate ) ->
            Position rawParam

        ( 2, NoImmediate ) ->
            Relative rawParam

        ( 0, DontCare ) ->
            Position rawParam

        ( 1, DontCare ) ->
            Immediate rawParam

        ( 2, DontCare ) ->
            Relative rawParam

        _ ->
            Debug.todo <| "Couldn't parse param mode " ++ String.fromInt mode


type Mask
    = DontCare
    | NoImmediate


type ToOp
    = Op0 Op
    | Op1 Mask (Parameter -> Op)
    | Op2 ( Mask, Mask ) (Parameter -> Parameter -> Op)
    | Op3 ( Mask, Mask, Mask ) (Parameter -> Parameter -> Parameter -> Op)


opcode1 : Int -> Mask -> (Parameter -> Op) -> Int -> Memory -> Op
opcode1 rawOpcode mask fn position mem =
    let
        rawParam =
            Memory.get (position + 1) mem

        parameter =
            param mask (digit 2 rawOpcode) rawParam
    in
    fn parameter


opcode2 : Int -> ( Mask, Mask ) -> (Parameter -> Parameter -> Op) -> Int -> Memory -> Op
opcode2 rawOpcode ( mask1, mask2 ) fn position mem =
    let
        rawParam1 =
            Memory.get (position + 1) mem

        rawParam2 =
            Memory.get (position + 2) mem

        parameter1 =
            param mask1 (digit 2 rawOpcode) rawParam1

        parameter2 =
            param mask2 (digit 3 rawOpcode) rawParam2
    in
    fn parameter1 parameter2


opcode3 : Int -> ( Mask, Mask, Mask ) -> (Parameter -> Parameter -> Parameter -> Op) -> Int -> Memory -> Op
opcode3 rawOpcode ( mask1, mask2, mask3 ) fn position mem =
    let
        rawParam1 =
            Memory.get (position + 1) mem

        rawParam2 =
            Memory.get (position + 2) mem

        rawParam3 =
            Memory.get (position + 3) mem

        parameter1 =
            param mask1 (digit 2 rawOpcode) rawParam1

        parameter2 =
            param mask2 (digit 3 rawOpcode) rawParam2

        parameter3 =
            param mask3 (digit 4 rawOpcode) rawParam3
    in
    fn parameter1 parameter2 parameter3


type Op
    = Add { addr0 : Parameter, addr1 : Parameter, dest : Parameter }
    | Mult { addr0 : Parameter, addr1 : Parameter, dest : Parameter }
    | Input { dest : Parameter }
    | Output { addr : Parameter }
    | JumpIfTrue { test : Parameter, jumpPosition : Parameter }
    | JumpIfFalse { test : Parameter, jumpPosition : Parameter }
    | LessThan { left : Parameter, right : Parameter, dest : Parameter }
    | Equals { left : Parameter, right : Parameter, dest : Parameter }
    | AddToRelativeBase { value : Parameter }
    | Halt


supportedOps : List ( Int, ToOp )
supportedOps =
    [ ( 1
      , Op3
            ( DontCare, DontCare, NoImmediate )
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
            ( DontCare, DontCare, NoImmediate )
            (\addr0 addr1 dest ->
                Mult
                    { addr0 = addr0
                    , addr1 = addr1
                    , dest = dest
                    }
            )
      )
    , ( 3, Op1 NoImmediate (\dest -> Input { dest = dest }) )
    , ( 4, Op1 DontCare (\addr -> Output { addr = addr }) )
    , ( 5
      , Op2 ( DontCare, DontCare )
            (\test jumpPosition ->
                JumpIfTrue
                    { test = test
                    , jumpPosition = jumpPosition
                    }
            )
      )
    , ( 6
      , Op2 ( DontCare, DontCare )
            (\test jumpPosition ->
                JumpIfFalse
                    { test = test
                    , jumpPosition = jumpPosition
                    }
            )
      )
    , ( 7
      , Op3 ( DontCare, DontCare, NoImmediate )
            (\left right dest ->
                LessThan
                    { left = left
                    , right = right
                    , dest = dest
                    }
            )
      )
    , ( 8
      , Op3 ( DontCare, DontCare, NoImmediate )
            (\left right dest ->
                Equals
                    { left = left
                    , right = right
                    , dest = dest
                    }
            )
      )
    , ( 9
      , Op1 DontCare
            (\value -> AddToRelativeBase { value = value })
      )
    , ( 99, Op0 Halt )
    ]


parseOpcode : Int -> Memory -> Maybe Op
parseOpcode position memory =
    let
        rawOpcode =
            Memory.get position memory

        opcode =
            rawOpcode |> remainderBy 100
    in
    parseOpcodeHelp
        ( rawOpcode, opcode )
        supportedOps
        position
        memory


parseOpcodeHelp : ( Int, Int ) -> List ( Int, ToOp ) -> Int -> Memory -> Maybe Op
parseOpcodeHelp ( rawOpcode, opcode ) supportedOps_ position mem =
    case supportedOps_ of
        [] ->
            Nothing

        ( wantedOpcode, op ) :: restOfSupportedOps ->
            if opcode == wantedOpcode then
                Just <|
                    case op of
                        Op0 a ->
                            a

                        Op1 mask fn ->
                            opcode1 rawOpcode mask fn position mem

                        Op2 masks fn ->
                            opcode2 rawOpcode masks fn position mem

                        Op3 masks fn ->
                            opcode3 rawOpcode masks fn position mem

            else
                parseOpcodeHelp
                    ( rawOpcode, opcode )
                    restOfSupportedOps
                    position
                    mem


{-|

    digit 0 == ones

    digit 1 == tens

    digit 2 == hundreds

Defaults to 0 if the number doesn't have the wanted digit.

-}
digit : Int -> Int -> Int
digit which n =
    (n |> modBy (10 ^ (which + 1))) // (10 ^ which)


type OutputError
    = NoOutputToGive


type Stop
    = UnknownOpcode Int Computer
    | Halted Computer
    | WaitsForInput Computer


step : Computer -> Result Stop Computer
step computer =
    parseOpcode computer.position computer.memory
        |> Result.fromMaybe
            (UnknownOpcode
                (Memory.get computer.position computer.memory)
                computer
            )
        |> Result.andThen (\op -> processOp op computer)


{-| Stopped by

  - HALT
  - needs input and doesn't have one
  - unknown opcode

-}
stepUntilStopped : Computer -> Result Stop Computer
stepUntilStopped computer =
    case step computer of
        Ok newComputer ->
            stepUntilStopped newComputer

        Err err ->
            Err err


unwrapParam : Parameter -> Int
unwrapParam parameter =
    case parameter of
        Immediate n ->
            n

        Position position ->
            position

        Relative position ->
            position


processOp : Op -> Computer -> Result Stop Computer
processOp op computer =
    case op of
        Add { addr0, addr1, dest } ->
            Ok <| processAdd addr0 addr1 dest computer

        Mult { addr0, addr1, dest } ->
            Ok <| processMult addr0 addr1 dest computer

        Input { dest } ->
            processInput dest computer

        Output { addr } ->
            Ok <| processOutput addr computer

        JumpIfTrue { test, jumpPosition } ->
            Ok <| processJumpIfTrue test jumpPosition computer

        JumpIfFalse { test, jumpPosition } ->
            Ok <| processJumpIfFalse test jumpPosition computer

        LessThan { left, right, dest } ->
            Ok <| processLessThan left right dest computer

        Equals { left, right, dest } ->
            Ok <| processEquals left right dest computer

        AddToRelativeBase { value } ->
            Ok <| processAddToRelativeBase value computer

        Halt ->
            Err <| Halted computer


processAdd : Parameter -> Parameter -> Parameter -> Computer -> Computer
processAdd addr0 addr1 dest computer =
    let
        num0 =
            Memory.getParam computer.relativeBase addr0 computer.memory

        num1 =
            Memory.getParam computer.relativeBase addr1 computer.memory

        newMemory =
            Memory.setParam computer.relativeBase dest (num0 + num1) computer.memory
    in
    computer
        |> setMemory newMemory
        |> jumpBy 4


processMult : Parameter -> Parameter -> Parameter -> Computer -> Computer
processMult addr0 addr1 dest computer =
    let
        num0 =
            Memory.getParam computer.relativeBase addr0 computer.memory

        num1 =
            Memory.getParam computer.relativeBase addr1 computer.memory

        newMemory =
            Memory.setParam computer.relativeBase dest (num0 * num1) computer.memory
    in
    computer
        |> setMemory newMemory
        |> jumpBy 4


processInput : Parameter -> Computer -> Result Stop Computer
processInput dest computer =
    let
        ( maybeInput, restOfInputs ) =
            Fifo.remove computer.inputs
    in
    maybeInput
        |> Result.fromMaybe (WaitsForInput computer)
        |> Result.map
            (\input ->
                let
                    newMemory =
                        Memory.setParam computer.relativeBase dest input computer.memory
                in
                computer
                    |> setMemory newMemory
                    |> setInputs restOfInputs
                    |> jumpBy 2
            )


processOutput : Parameter -> Computer -> Computer
processOutput addr computer =
    let
        output =
            Memory.getParam computer.relativeBase addr computer.memory
    in
    computer
        |> addOutput output
        |> jumpBy 2


processJumpIfTrue : Parameter -> Parameter -> Computer -> Computer
processJumpIfTrue test jumpPosition computer =
    let
        testValue =
            Memory.getParam computer.relativeBase test computer.memory

        newPosition =
            if testValue /= 0 then
                Memory.getParam computer.relativeBase jumpPosition computer.memory

            else
                computer.position + 3
    in
    computer
        |> jumpTo newPosition


processJumpIfFalse : Parameter -> Parameter -> Computer -> Computer
processJumpIfFalse test jumpPosition computer =
    let
        testValue =
            Memory.getParam computer.relativeBase test computer.memory

        newPosition =
            if testValue == 0 then
                Memory.getParam computer.relativeBase jumpPosition computer.memory

            else
                computer.position + 3
    in
    computer
        |> jumpTo newPosition


processLessThan : Parameter -> Parameter -> Parameter -> Computer -> Computer
processLessThan left right dest computer =
    let
        leftValue =
            Memory.getParam computer.relativeBase left computer.memory

        rightValue =
            Memory.getParam computer.relativeBase right computer.memory

        result =
            if leftValue < rightValue then
                1

            else
                0

        newMemory =
            Memory.setParam computer.relativeBase dest result computer.memory
    in
    computer
        |> jumpBy 4
        |> setMemory newMemory


processEquals : Parameter -> Parameter -> Parameter -> Computer -> Computer
processEquals left right dest computer =
    let
        leftValue =
            Memory.getParam computer.relativeBase left computer.memory

        rightValue =
            Memory.getParam computer.relativeBase right computer.memory

        result =
            if leftValue == rightValue then
                1

            else
                0

        newMemory =
            Memory.setParam computer.relativeBase dest result computer.memory
    in
    computer
        |> jumpBy 4
        |> setMemory newMemory


processAddToRelativeBase : Parameter -> Computer -> Computer
processAddToRelativeBase value computer =
    let
        value_ =
            Memory.getParam computer.relativeBase value computer.memory

        newRelativeBase =
            computer.relativeBase + value_
    in
    computer
        |> jumpBy 2
        |> setRelativeBase newRelativeBase
