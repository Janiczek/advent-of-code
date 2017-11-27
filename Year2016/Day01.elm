module Year2016.Day01 exposing (..)

import Advent exposing (Test1, Test2)
import String


main : Program Never ( Output1, Output2 ) Never
main =
    Advent.program
        { input = input
        , parse = parse
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
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


type alias Output1 =
    Distance


type alias Output2 =
    Maybe Distance


input : String
input =
    "R3, L5, R2, L2, R1, L3, R1, R3, L4, R3, L1, L1, R1, L3, R2, L3, L2, R1, R1, L1, R4, L1, L4, R3, L2, L2, R1, L1, R5, R4, R2, L5, L2, R5, R5, L2, R3, R1, R1, L3, R1, L4, L4, L190, L5, L2, R4, L5, R4, R5, L4, R1, R2, L5, R50, L2, R1, R73, R1, L2, R191, R2, L4, R1, L5, L5, R5, L3, L5, L4, R4, R5, L4, R4, R4, R5, L2, L5, R3, L4, L4, L5, R2, R2, R2, R4, L3, R4, R5, L3, R5, L2, R3, L1, R2, R2, L3, L1, R5, L3, L5, R2, R4, R1, L1, L5, R3, R2, L3, L4, L5, L1, R3, L5, L2, R2, L3, L4, L1, R1, R4, R2, R2, R4, R2, R2, L3, L3, L4, R4, L4, L4, R1, L4, L4, R1, L2, R5, R2, R3, R3, L2, L5, R3, L3, R5, L2, R3, R2, L4, L3, L1, R2, L2, L3, L5, R3, L1, L3, L4, L3"


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


compute1 : Input -> Output1
compute1 input =
    input
        |> List.foldl
            (\{ rotation, amount } ( orientation, position ) ->
                let
                    newOrientation =
                        orientation |> rotate rotation

                    newPosition =
                        position |> move newOrientation amount
                in
                    ( newOrientation, newPosition )
            )
            ( North, initialPosition )
        |> Tuple.second
        |> distance


moves : Orientation -> Int -> Position -> List Position
moves orientation amount position =
    ( orientation, 1 )
        |> List.repeat amount
        |> List.scanl (\( orientation, amount ) position -> move orientation amount position) position
        |> List.tail
        |> Maybe.withDefault []


compute2 : Input -> Output2
compute2 input =
    let
        ( history, _, _ ) =
            input
                |> List.foldl
                    (\{ rotation, amount } ( history, orientation, position ) ->
                        let
                            newOrientation =
                                orientation |> rotate rotation

                            newHistory =
                                history ++ moves newOrientation amount position

                            newPosition =
                                position |> move newOrientation amount
                        in
                            ( newHistory, newOrientation, newPosition )
                    )
                    ( [ initialPosition ], North, initialPosition )
    in
        history
            |> List.filter (\position -> (history |> List.filter (\x -> x == position) |> List.length) > 1)
            |> List.head
            |> Maybe.map distance


tests1 : List (Test1 Input Output1)
tests1 =
    [ Test1 "example 1"
        "R2, L3"
        [ Step Right 2
        , Step Left 3
        ]
        5
    , Test1 "example 2"
        "R2, R2, R2"
        [ Step Right 2
        , Step Right 2
        , Step Right 2
        ]
        2
    , Test1 "example 3"
        "R5, L5, R5, R3"
        [ Step Right 5
        , Step Left 5
        , Step Right 5
        , Step Right 3
        ]
        12
    ]


tests2 : List (Test2 Output2)
tests2 =
    [ Test2 "example 1"
        "R8, R4, R4, R8"
        (Just 4)
    ]
