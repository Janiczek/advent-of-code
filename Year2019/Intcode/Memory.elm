module Year2019.Intcode.Memory exposing
    ( Memory
    , fromList
    , fromString
    , get
    , getParam
    , set
    , setMany
    , setParam
    )

import Advent
import Array exposing (Array)
import Dict exposing (Dict)
import Maybe.Extra
import Year2019.Intcode.Parameter as Parameter exposing (Parameter(..))


type alias Memory =
    { program : Array Int
    , programLength : Int
    , extra : Dict Int Int
    }


fromList : List Int -> Memory
fromList list =
    { program = Array.fromList list
    , programLength = List.length list
    , extra = Dict.empty
    }


{-| Needs CSV:

    1,2,3,4,5

-}
fromString : String -> Maybe Memory
fromString string =
    string
        |> String.split ","
        |> List.map String.toInt
        |> Maybe.Extra.combine
        |> Maybe.map fromList


get : Int -> Memory -> Int
get position mem =
    if position < mem.programLength then
        Array.get position mem.program
            |> Advent.unsafeMaybe "Memory.get - shouldn't happen"

    else
        Dict.get position mem.extra
            |> Maybe.withDefault 0


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


setMany : List ( Int, Int ) -> Memory -> Memory
setMany list mem =
    List.foldl
        (\( position, value ) mem_ -> set position value mem_)
        mem
        list
