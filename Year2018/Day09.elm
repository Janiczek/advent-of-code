module Year2018.Day09 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)
import List.Zipper as Zipper exposing (Zipper)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    { players : Int
    , lastMarble : Value
    }


type alias Input2 =
    Input1


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    case String.words string of
        [ playersString, _, _, _, _, _, pointsString, _ ] ->
            Input1
                (Advent.unsafeToInt playersString)
                (Advent.unsafeToInt pointsString)

        _ ->
            Debug.todo "wrong input 1"


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


type alias Player =
    Int


type alias Value =
    Int


type alias Score =
    Int


compute1 : Input1 -> Output1
compute1 input =
    play 1 input (Zipper.singleton 0) Dict.empty


play : Value -> Input1 -> Zipper Value -> Dict Player Score -> Score
play marbleToBePut ({ players, lastMarble } as input) marbles points =
    if marbleToBePut > lastMarble then
        points
            |> Dict.values
            |> List.maximum
            |> Advent.unsafeMaybe

    else
        let
            currentPlayer =
                marbleToBePut
                    |> modBy players
        in
        if (marbleToBePut |> modBy 23) == 0 then
            let
                ( newMarbles, removedMarble ) =
                    remove7CounterClockwise marbles

                newPoints =
                    points
                        |> addToPlayer currentPlayer marbleToBePut
                        |> addToPlayer currentPlayer removedMarble
            in
            play (marbleToBePut + 1) input newMarbles newPoints

        else
            let
                newMarbles =
                    insertUsually marbleToBePut marbles
            in
            play (marbleToBePut + 1) input newMarbles points


circularNext : Zipper a -> Zipper a
circularNext zipper =
    case Zipper.next zipper of
        Just z ->
            z

        Nothing ->
            Zipper.first zipper


circularPrevious : Zipper a -> Zipper a
circularPrevious zipper =
    case Zipper.previous zipper of
        Just z ->
            z

        Nothing ->
            Zipper.last zipper


appendToRight : a -> Zipper a -> Zipper a
appendToRight value zipper =
    Zipper.mapAfter (\list -> value :: list) zipper


removeNext : Zipper a -> Zipper a
removeNext zipper =
    if List.isEmpty (Zipper.after zipper) then
        Zipper.mapBefore (List.drop 1) zipper

    else
        Zipper.mapAfter (List.drop 1) zipper


insertUsually : Int -> Zipper Value -> Zipper Value
insertUsually marble marbles =
    marbles
        |> circularNext
        |> appendToRight marble
        |> circularNext


remove7CounterClockwise : Zipper Value -> ( Zipper Value, Value )
remove7CounterClockwise marbles =
    let
        afterMoving =
            marbles
                |> circularPrevious
                |> circularPrevious
                |> circularPrevious
                |> circularPrevious
                |> circularPrevious
                |> circularPrevious
                |> circularPrevious

        current =
            Zipper.current afterMoving

        afterRemoving =
            afterMoving
                |> circularPrevious
                |> removeNext
                |> circularNext
    in
    ( afterRemoving, current )


addToPlayer : Int -> Int -> Dict Int Int -> Dict Int Int
addToPlayer currentPlayer marble points =
    points
        |> Dict.update currentPlayer
            (\maybeScore ->
                case maybeScore of
                    Just score ->
                        Just (score + marble)

                    Nothing ->
                        Just marble
            )


compute2 : Input2 -> Output2
compute2 input =
    ---1
    compute1 { input | lastMarble = input.lastMarble * 100 }



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "example 6" "30 players; last marble is worth 5807 points" (Input1 30 5807) 37305
    , Test "example 5" "21 players; last marble is worth 6111 points" (Input1 21 6111) 54718
    , Test "example 4" "17 players; last marble is worth 1104 points" (Input1 17 1104) 2764
    , Test "example 3" "13 players; last marble is worth 7999 points" (Input1 13 7999) 146373
    , Test "example 2" "10 players; last marble is worth 1618 points" (Input1 10 1618) 8317
    , Test "example" "9 players; last marble is worth 25 points" (Input1 9 25) 32
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
452 players; last marble is worth 71250 points
"""
        |> Advent.removeNewlinesAtEnds


main : Program () ( Output1, Output2 ) Never
main =
    Advent.program
        { input = input_
        , parse1 = parse1
        , parse2 = parse2
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }
