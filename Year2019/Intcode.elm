module Year2019.Intcode exposing
    ( Error(..)
    , Mask(..)
    , Memory
    , Op(..)
    , Parameter(..)
    , digit
    , get
    , getParam
    , getSafe
    , init
    , memoryFromList
    , opcodeSafe1
    , opcodeSafe2
    , opcodeSafe3
    , parse
    , parseSafe
    , parseWith
    , set
    , setParam
    , step
    , unwrapParam
    )

{- Intcode programs:

      2019-02
      2019-05 (position/immediate, new opcodes)
      2019-07
      2019-09 (relative)

   TODO: demand (Position : Maybe Int, Memory) from all the `process` functions;
   let them have their own metadata? (Every program that has an Output
   instruction needs also the Maybe LogEntry thing.)

   TODO: common type for (Log, RelativeBase, Position, Memory)
   ... and maybe (List Input)

-}

import Advent
import Array exposing (Array)
import Dict exposing (Dict)
import Maybe.Extra


type alias Memory =
    { program : Array Int
    , programLength : Int
    , extra : Dict Int Int
    }


type Error
    = UnknownOpcode
        { position : Int
        , value : Int
        }


type Parameter
    = Immediate Int
    | Position Int
    | Relative Int


{-| Needs CSV:

    1,2,3,4,5

-}
parse : String -> Memory
parse string =
    string
        |> String.split ","
        |> List.map Advent.unsafeToInt
        |> memoryFromList


parseSafe : String -> Maybe Memory
parseSafe string =
    string
        |> String.split ","
        |> List.map String.toInt
        |> Maybe.Extra.combine
        |> Maybe.map memoryFromList


param : Mask -> Int -> Int -> Parameter
param mask mode rawParam =
    case ( mode, mask ) of
        ( 0, WantPosition ) ->
            Position rawParam

        ( 1, WantPosition ) ->
            Position rawParam

        ( 2, WantPosition ) ->
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
    | WantPosition


type Op a
    = Op0 a
    | Op1 Mask (Parameter -> a)
    | Op2 ( Mask, Mask ) (Parameter -> Parameter -> a)
    | Op3 ( Mask, Mask, Mask ) (Parameter -> Parameter -> Parameter -> a)


opcode1 : Int -> Mask -> (Parameter -> a) -> Int -> Memory -> a
opcode1 rawOpcode mask fn position mem =
    let
        rawParam =
            get (position + 1) mem

        parameter =
            param mask (digit 2 rawOpcode) rawParam
    in
    fn parameter


opcode2 : Int -> ( Mask, Mask ) -> (Parameter -> Parameter -> a) -> Int -> Memory -> a
opcode2 rawOpcode ( mask1, mask2 ) fn position mem =
    let
        rawParam1 =
            get (position + 1) mem

        rawParam2 =
            get (position + 2) mem

        parameter1 =
            param mask1 (digit 2 rawOpcode) rawParam1

        parameter2 =
            param mask2 (digit 3 rawOpcode) rawParam2
    in
    fn parameter1 parameter2


opcode3 : Int -> ( Mask, Mask, Mask ) -> (Parameter -> Parameter -> Parameter -> a) -> Int -> Memory -> a
opcode3 rawOpcode ( mask1, mask2, mask3 ) fn position mem =
    let
        rawParam1 =
            get (position + 1) mem

        rawParam2 =
            get (position + 2) mem

        rawParam3 =
            get (position + 3) mem

        parameter1 =
            param mask1 (digit 2 rawOpcode) rawParam1

        parameter2 =
            param mask2 (digit 3 rawOpcode) rawParam2

        parameter3 =
            param mask3 (digit 4 rawOpcode) rawParam3
    in
    fn parameter1 parameter2 parameter3


opcodeSafe1 : Int -> Mask -> (Parameter -> a) -> Int -> Memory -> Maybe a
opcodeSafe1 rawOpcode mask fn position mem =
    getSafe (position + 1) mem
        |> Maybe.map
            (\rawParam ->
                let
                    parameter =
                        param mask (digit 2 rawOpcode) rawParam
                in
                fn parameter
            )


opcodeSafe2 : Int -> ( Mask, Mask ) -> (Parameter -> Parameter -> a) -> Int -> Memory -> Maybe a
opcodeSafe2 rawOpcode ( mask1, mask2 ) fn position mem =
    Maybe.map2
        (\rawParam1 rawParam2 ->
            let
                parameter1 =
                    param mask1 (digit 2 rawOpcode) rawParam1

                parameter2 =
                    param mask2 (digit 3 rawOpcode) rawParam2
            in
            fn parameter1 parameter2
        )
        (getSafe (position + 1) mem)
        (getSafe (position + 2) mem)


opcodeSafe3 : Int -> ( Mask, Mask, Mask ) -> (Parameter -> Parameter -> Parameter -> a) -> Int -> Memory -> Maybe a
opcodeSafe3 rawOpcode ( mask1, mask2, mask3 ) fn position mem =
    Maybe.map3
        (\rawParam1 rawParam2 rawParam3 ->
            let
                parameter1 =
                    param mask1 (digit 2 rawOpcode) rawParam1

                parameter2 =
                    param mask2 (digit 3 rawOpcode) rawParam2

                parameter3 =
                    param mask3 (digit 4 rawOpcode) rawParam3
            in
            fn parameter1 parameter2 parameter3
        )
        (getSafe (position + 1) mem)
        (getSafe (position + 2) mem)
        (getSafe (position + 3) mem)


parseWith : List ( Int, Op a ) -> Int -> Memory -> Maybe a
parseWith supportedOps position memory =
    let
        rawOpcode =
            get position memory

        opcode =
            rawOpcode |> remainderBy 100
    in
    parseWithHelp
        ( rawOpcode, opcode )
        supportedOps
        position
        memory


parseWithHelp : ( Int, Int ) -> List ( Int, Op a ) -> Int -> Memory -> Maybe a
parseWithHelp ( rawOpcode, opcode ) supportedOps position mem =
    case supportedOps of
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
                parseWithHelp ( rawOpcode, opcode ) restOfSupportedOps position mem


{-|

    digit 0 == ones

    digit 1 == tens

    digit 2 == hundreds

-}
digit : Int -> Int -> Int
digit which n =
    -- TODO maybe make it signal that we're past the number now (digit 2 99)
    (n |> modBy (10 ^ (which + 1))) // (10 ^ which)


init : List ( Int, Int ) -> Memory -> Memory
init list mem =
    List.foldl
        (\( position, value ) mem_ -> set position value mem_)
        mem
        list


step :
    (Int -> Memory -> Maybe op)
    -> (op -> Int -> Int -> Memory -> a)
    -> Int
    -> Int
    -> Memory
    -> Result Error a
step parseOpcode processOp relativeBase position mem =
    parseOpcode position mem
        |> Result.fromMaybe
            (UnknownOpcode
                { position = position
                , value = get position mem
                }
            )
        |> Result.map (\op -> processOp op relativeBase position mem)


get : Int -> Memory -> Int
get position mem =
    if position < mem.programLength then
        Array.get position mem.program
            |> Advent.unsafeMaybe

    else
        Dict.get position mem.extra
            |> Maybe.withDefault 0


getSafe : Int -> Memory -> Maybe Int
getSafe position mem =
    if position < mem.programLength then
        Array.get position mem.program

    else
        Dict.get position mem.extra


getParam : Int -> Parameter -> Memory -> Int
getParam relativeBase parameter mem =
    case parameter of
        Immediate n ->
            n

        Position position ->
            get position mem

        Relative position ->
            get (relativeBase + position) mem


set : Int -> Int -> Memory -> Memory
set position value mem =
    if position < mem.programLength then
        { mem | program = Array.set position value mem.program }

    else
        { mem | extra = Dict.insert position value mem.extra }


setParam : Int -> Parameter -> Int -> Memory -> Memory
setParam relativeBase parameter value mem =
    case parameter of
        Immediate _ ->
            Debug.todo "Can't write to an immediate position, likely error in Mask"

        Position position ->
            set position value mem

        Relative position ->
            set (relativeBase + position) value mem


unwrapParam : Parameter -> Int
unwrapParam parameter =
    case parameter of
        Immediate n ->
            n

        Position position ->
            position

        Relative position ->
            -- TODO is this right?
            position


memoryFromList : List Int -> Memory
memoryFromList list =
    { program = Array.fromList list
    , programLength = List.length list
    , extra = Dict.empty
    }
