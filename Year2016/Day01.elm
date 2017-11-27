module Year2016.Day01 exposing (..)

import Advent exposing (Test)
import String


main : Program Never Output Never
main =
    Advent.program
        { input = input
        , parse = parse
        , compute = compute
        , tests = tests
        }


type alias Input =
    List Step


type alias Step =
    { rotation : Rotation
    , amount : Int
    }


type Rotation
    = Left
    | Right


type alias Position =
    { x : Int
    , y : Int
    }


type alias Distance =
    Int


type Orientation
    = North
    | East
    | South
    | West


initialPosition : Position
initialPosition =
    Position 0 0


rotate : Rotation -> Orientation -> Orientation
rotate rotation orientation =
    case rotation of
        Left ->
            case orientation of
                North ->
                    West

                West ->
                    South

                South ->
                    East

                East ->
                    North

        Right ->
            case orientation of
                North ->
                    East

                East ->
                    South

                South ->
                    West

                West ->
                    North


move : Orientation -> Int -> Position -> Position
move orientation amount { x, y } =
    case orientation of
        North ->
            Position x (y + amount)

        East ->
            Position (x + amount) y

        South ->
            Position x (y - amount)

        West ->
            Position (x - amount) y


distance : Position -> Distance
distance { x, y } =
    abs x + abs y


type alias Output =
    Distance


input : String
input =
    "R2, L3"


parse : String -> Input
parse string =
    string
        |> String.split ", "
        |> List.map parseStep


parseStep : String -> Step
parseStep string =
    let
        ( rotation, amountString ) =
            case String.uncons string of
                Just ( 'L', amountString ) ->
                    ( Left, amountString )

                Just ( 'R', amountString ) ->
                    ( Right, amountString )

                _ ->
                    Debug.crash "Wrong input!"
    in
        amountString
            |> String.toInt
            |> Result.map (\amount -> Step rotation amount)
            |> Result.mapError (\x -> Debug.crash "Wrong input!")
            |> Result.withDefault (Step Left 0)


compute : Input -> Output
compute input =
    input
        |> List.foldl
            (\{ rotation, amount } ( orientation, position ) ->
                let
                    newOrientation =
                        orientation |> rotate rotation

                    newPosition =
                        position |> move orientation amount
                in
                    ( newOrientation, newPosition )
            )
            ( North, initialPosition )
        |> Tuple.second
        |> distance


tests : List (Test Input Output)
tests =
    [ Test "example 1"
        "R2, L3"
        [ Step Right 2
        , Step Left 3
        ]
        5
    , Test "example 2"
        "R2, R2, R2"
        [ Step Right 2
        , Step Right 2
        , Step Right 2
        ]
        2
    ]
