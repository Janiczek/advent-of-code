module Year2019.Intcode.Disasm exposing (Data(..), disassembleWith)

import Array
import Year2019.Intcode as Intcode exposing (Memory, Op(..))


type Data a
    = Instruction a
    | Data Int


disassembleWith : List ( Int, Op a ) -> Memory -> List ( Int, Data a )
disassembleWith supportedOps mem =
    disassembleWithHelp [] supportedOps 0 (Array.length mem) mem


disassembleWithHelp : List ( Int, Data a ) -> List ( Int, Op a ) -> Int -> Int -> Memory -> List ( Int, Data a )
disassembleWithHelp listSoFar supportedOps position length mem =
    if position >= length then
        List.reverse listSoFar

    else
        case disassembleOne supportedOps position mem of
            Nothing ->
                let
                    newList =
                        ( position, Data (Intcode.get position mem) )
                            :: listSoFar

                    newPosition =
                        position + 1
                in
                disassembleWithHelp newList supportedOps newPosition length mem

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
                disassembleWithHelp newList supportedOps newPosition length mem


disassembleOne : List ( Int, Op a ) -> Int -> Memory -> Maybe ( a, Op a )
disassembleOne supportedOps position mem =
    let
        rawOpcode =
            Intcode.get position mem

        opcode =
            rawOpcode |> remainderBy 100
    in
    disassembleOneHelp
        ( rawOpcode, opcode )
        supportedOps
        position
        mem


disassembleOneHelp : ( Int, Int ) -> List ( Int, Op a ) -> Int -> Memory -> Maybe ( a, Op a )
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
                        Intcode.opcodeSafe1 rawOpcode mask fn position mem

                    Op2 masks fn ->
                        Intcode.opcodeSafe2 rawOpcode masks fn position mem

                    Op3 masks fn ->
                        Intcode.opcodeSafe3 rawOpcode masks fn position mem
                )
                    |> Maybe.map (\parsedOp -> ( parsedOp, op ))

            else
                disassembleOneHelp ( rawOpcode, opcode ) restOfSupportedOps position mem
