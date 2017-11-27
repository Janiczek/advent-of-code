module Year2016.Day01 exposing (..)

import Advent exposing (Test(..))
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


type Step
    = Left Int
    | Right Int


type alias Position =
    { north : Int, east : Int }


type alias Distance =
    Int


initialPosition : Position
initialPosition =
    Position 0 0


distance : Position -> Distance
distance { north, east } =
    abs north + abs east


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
        ( stepType, amountString ) =
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
            |> Result.map (\amount -> stepType amount)
            |> Result.mapError (\x -> Debug.crash "Wrong input!")
            |> Result.withDefault (Left 0)


compute : Input -> Output
compute input =
    10


tests : List (Test Input Output)
tests =
    [ ParseTest "example 1"
        "R2, L3"
        [ Right 2, Left 3 ]
    , ComputeTest "example 1"
        [ Right 2, Left 3 ]
        5
    ]
