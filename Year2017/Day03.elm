module Year2017.Day03 exposing (..)

import Advent exposing (Test)
import Dict exposing (Dict)


main : Program Never ( Output1, Output2 ) Never
main =
    Advent.program
        { input = input
        , parse1 = parse1
        , parse2 = parse2
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }


distance : ( Int, Int ) -> Int
distance ( x, y ) =
    abs x + abs y



--ulam : Int -> Dict Int ( Int, Int )
--ulam n =
--    List.range 1 n
--        -- [1,2,3,4,5]
--        |> List.foldl
--    8 * n
-- layer:                 0, 1, 2,  3,  4, ...
-- numbers:               1, 8, 16, 24,         = if n == 0 then 1 else 8 * n
-- starting number at SE: 1, 2, 10, 26, 50      = List.sum (List.map numbers (List.range 1 n)) + 1


ringSize : Int -> Int
ringSize n =
    if n == 0 then
        1
    else
        8 * n


southEastNumber : Int -> Int
southEastNumber n =
    if n == 0 then
        1
    else
        List.range 0 (n - 1)
            |> List.map ringSize
            |> List.sum
            |> (\x -> x + 1)


ringFor : Int -> Int
ringFor n =
    let
        ringForAux n guess =
            if southEastNumber guess > n then
                guess - 1
            else
                ringForAux n (guess + 1)
    in
        ringForAux n 0


sideSize : Int -> Int
sideSize layer =
    if layer == 0 then
        0
    else
        2 * layer


southEastCoordinate : Int -> ( Int, Int )
southEastCoordinate layer =
    if layer == 0 then
        ( 0, 0 )
    else
        ( layer, -layer + 1 )


type alias Input1 =
    Int


type alias Input2 =
    Int


type alias Output1 =
    Int


type alias Output2 =
    Int


input : String
input =
    "289326"


parse1 : String -> Input1
parse1 input =
    input
        |> Advent.toInt


parse2 : String -> Input2
parse2 input =
    input
        |> Advent.toInt


coords : Int -> ( Int, Int )
coords n =
    let
        ringNumber =
            ringFor n

        stepsNeeded =
            (n - southEastNumber ringNumber)

        size =
            sideSize ringNumber

        ( seX, seY ) =
            southEastCoordinate ringNumber
    in
        if n == 1 then
            ( 0, 0 )
        else if stepsNeeded < size then
            let
                stepsNorth =
                    stepsNeeded
            in
                ( seX, seY + stepsNorth )
        else if stepsNeeded < size * 2 - 1 then
            let
                stepsNorth =
                    size - 1

                stepsWest =
                    stepsNeeded - stepsNorth
            in
                ( seX - stepsWest, seY + stepsNorth )
        else if stepsNeeded < size * 3 - 1 then
            let
                stepsNorth =
                    size - 1

                stepsWest =
                    size

                stepsSouth =
                    stepsNeeded - stepsWest - stepsNorth
            in
                ( seX - stepsWest, seY + stepsNorth - stepsSouth )
        else if stepsNeeded <= size * 4 - 1 then
            let
                stepsNorth =
                    size - 1

                stepsWest =
                    size

                stepsSouth =
                    size

                stepsEast =
                    stepsNeeded - stepsSouth - stepsWest - stepsNorth
            in
                ( seX - stepsWest + stepsEast, seY + stepsNorth - stepsSouth )
        else
            Debug.crash "error somewhere"


compute1 : Input1 -> Output1
compute1 input =
    input
        |> coords
        |> distance


allNeighbours : ( Int, Int ) -> List ( Int, Int )
allNeighbours ( x, y ) =
    [ ( x - 1, y + 1 )
    , ( x, y + 1 )
    , ( x + 1, y + 1 )
    , ( x - 1, y )
    , ( x + 1, y )
    , ( x - 1, y - 1 )
    , ( x, y - 1 )
    , ( x + 1, y - 1 )
    ]


neighboursSum : Int -> Dict ( Int, Int ) Int -> Int
neighboursSum n storedNumbers =
    if n == 1 then
        1
    else
        let
            ( nx, ny ) =
                coords n

            allNeighs =
                allNeighbours ( nx, ny )
        in
            allNeighs
                |> List.filterMap (\coord -> Dict.get coord storedNumbers)
                |> List.sum



-- neighbours 1 = []
-- neighbours 2 = [ 1 ]
-- neighbours 3 = [ 1, 2 ]
-- neighbours 5 = [ 1, 4 ]


compute2 : Input2 -> Output2
compute2 input =
    takeAStep input 1 (Dict.singleton ( 0, 0 ) 1)


takeAStep : Int -> Int -> Dict ( Int, Int ) Int -> Int
takeAStep stoppingPoint n storedNumbers =
    let
        sum =
            neighboursSum n storedNumbers

        cs =
            coords n
    in
        if sum > stoppingPoint then
            sum
        else
            let
                newStoredNumbers =
                    storedNumbers
                        |> Dict.insert cs sum
            in
                takeAStep stoppingPoint (n + 1) newStoredNumbers


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "example 1" "1" 1 0
    , Test "example 2" "12" 12 3
    , Test "example 3" "23" 23 2
    , Test "example 4" "1024" 1024 31
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []
