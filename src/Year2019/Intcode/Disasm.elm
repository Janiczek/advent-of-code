module Year2019.Intcode.Disasm exposing (Data(..), disassemble)

import Array
import Year2019.Intcode as Intcode exposing (Op(..), ToOp(..))
import Year2019.Intcode.Memory as Memory exposing (Memory)


type Data
    = Instruction Op
    | Data Int


disassemble : Memory -> List ( Int, Data )
disassemble mem =
    disassembleHelp [] Intcode.supportedOps 0 (Memory.length mem) mem


disassembleHelp : List ( Int, Data ) -> List ( Int, ToOp ) -> Int -> Int -> Memory -> List ( Int, Data )
disassembleHelp listSoFar supportedOps position length mem =
    if position >= length then
        List.reverse listSoFar

    else
        case disassembleOne supportedOps position mem of
            Nothing ->
                let
                    newList =
                        ( position, Data (Memory.get position mem) )
                            :: listSoFar

                    newPosition =
                        position + 1
                in
                disassembleHelp newList supportedOps newPosition length mem

            Just ( a, op ) ->
                let
                    newList =
                        ( position, Instruction a )
                            :: listSoFar

                    newPosition =
                        case op of
                            Op0 _ ->
                                position + 1

                            Op1 _ _ ->
                                position + 2

                            Op2 _ _ ->
                                position + 3

                            Op3 _ _ ->
                                position + 4
                in
                disassembleHelp newList supportedOps newPosition length mem


disassembleOne : List ( Int, ToOp ) -> Int -> Memory -> Maybe ( Op, ToOp )
disassembleOne supportedOps position mem =
    let
        rawOpcode =
            Memory.get position mem

        opcode =
            rawOpcode |> remainderBy 100
    in
    disassembleOneHelp
        ( rawOpcode, opcode )
        supportedOps
        position
        mem


disassembleOneHelp : ( Int, Int ) -> List ( Int, ToOp ) -> Int -> Memory -> Maybe ( Op, ToOp )
disassembleOneHelp ( rawOpcode, opcode ) supportedOps position mem =
    case supportedOps of
        [] ->
            Nothing

        ( wantedOpcode, op ) :: restOfSupportedOps ->
            if opcode == wantedOpcode then
                (case op of
                    Op0 a ->
                        Just a

                    Op1 mask fn ->
                        Intcode.opcode1 rawOpcode mask fn position mem

                    Op2 masks fn ->
                        Intcode.opcode2 rawOpcode masks fn position mem

                    Op3 masks fn ->
                        Intcode.opcode3 rawOpcode masks fn position mem
                )
                    |> Maybe.map (\op_ -> ( op_, op ))

            else
                disassembleOneHelp ( rawOpcode, opcode ) restOfSupportedOps position mem
