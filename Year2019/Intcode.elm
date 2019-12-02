module Year2019.Intcode exposing
    ( Memory
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


step : (Int -> Memory -> op) -> (op -> Int -> Memory -> a) -> Int -> Memory -> a
step parseOpcode processOp position mem =
    let
        op : op
        op =
            parseOpcode position mem
    in
    processOp op position mem


get : Int -> Memory -> Int
get position mem =
    Array.get position mem
        |> Advent.unsafeMaybe
