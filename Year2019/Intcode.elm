module Year2019.Intcode exposing
    ( Error(..)
    , Memory
    , Position
    , Value
    , get
    , init
    , parse
    , step
    )

import Advent
import Array exposing (Array)


type alias Memory =
    Array Int


type alias Position =
    Int


type alias Value =
    Int


type Error
    = UnknownOpcode
        { position : Position
        , value : Value
        }


{-| Needs CSV:

    1,2,3,4,5

-}
parse : String -> Memory
parse string =
    string
        |> String.split ","
        |> List.map Advent.unsafeToInt
        |> Array.fromList


init : List ( Position, Value ) -> Memory -> Memory
init list mem =
    List.foldl
        (\( position, value ) mem_ ->
            Array.set position value mem_
        )
        mem
        list


step :
    (Int -> Memory -> Maybe op)
    -> (op -> Int -> Memory -> a)
    -> Int
    -> Memory
    -> Result Error a
step parseOpcode processOp position mem =
    parseOpcode position mem
        |> Result.fromMaybe
            (UnknownOpcode
                { position = position
                , value = get position mem
                }
            )
        |> Result.map (\op -> processOp op position mem)


get : Int -> Memory -> Int
get position mem =
    Array.get position mem
        |> Advent.unsafeMaybe
