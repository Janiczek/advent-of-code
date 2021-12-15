module Year2018.Day11 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Array exposing (Array)
import List.Extra



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    Int


type alias Input2 =
    Int


type alias Output1 =
    String


type alias Output2 =
    String



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    Advent.unsafeToInt string


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    let
        powerLevel : Int -> Int -> Int
        powerLevel x y =
            (((((x + 10) * y + input) * (x + 10)) // 100) |> modBy 10) - 5

        topLeft : Array (Array ( Int, Int, Int )) -> ( Int, Int )
        topLeft grid =
            grid
                |> Array.get 0
                |> Maybe.andThen (Array.get 0)
                |> Advent.unsafeMaybe
                |> (\( x, y, _ ) -> ( x, y ))

        coordinate : ( Int, Int ) -> String
        coordinate ( x, y ) =
            String.fromInt x ++ "," ++ String.fromInt y

        limit : Int
        limit =
            300

        powerGrid : Array (Array ( Int, Int, Int ))
        powerGrid =
            Array.initialize limit (\y -> Array.initialize limit (\x -> ( x + 1, y + 1, powerLevel (x + 1) (y + 1) )))

        subgrids : List (Array (Array ( Int, Int, Int )))
        subgrids =
            List.range 0 (limit - 3)
                |> List.Extra.andThen
                    (\x ->
                        List.range 0 (limit - 3)
                            |> List.Extra.andThen
                                (\y ->
                                    [ powerGrid
                                        |> Array.slice y (y + 3)
                                        |> Array.map (Array.slice x (x + 3))
                                    ]
                                )
                    )

        scoreSubgrid : Array (Array ( Int, Int, Int )) -> Int
        scoreSubgrid subgrid =
            Array.foldl (\row acc -> acc + Array.foldl (\( _, _, value ) acc_ -> acc_ + value) 0 row) 0 subgrid

        maxSubgrid : Array (Array ( Int, Int, Int ))
        maxSubgrid =
            subgrids
                |> List.Extra.maximumBy scoreSubgrid
                |> Advent.unsafeMaybe
    in
    maxSubgrid
        |> topLeft
        |> coordinate


compute2 : Input2 -> Output2
compute2 input =
    let
        powerLevel : Int -> Int -> Int
        powerLevel x y =
            (((((x + 10) * y + input) * (x + 10)) // 100) |> modBy 10) - 5

        topLeft : Array (Array ( ( Int, Int, Int ), Int )) -> ( Int, Int, Int )
        topLeft grid =
            grid
                |> Array.get 0
                |> Maybe.andThen (Array.get 0)
                |> Advent.unsafeMaybe
                |> Tuple.first

        coordinate : ( Int, Int, Int ) -> String
        coordinate ( x, y, size ) =
            String.fromInt x ++ "," ++ String.fromInt y ++ "," ++ String.fromInt size

        limit : Int
        limit =
            300

        powerGrid : Array (Array ( Int, Int, Int ))
        powerGrid =
            Array.initialize limit (\y -> Array.initialize limit (\x -> ( x + 1, y + 1, powerLevel (x + 1) (y + 1) )))

        subgrids : Int -> List (Array (Array ( ( Int, Int, Int ), Int )))
        subgrids squareSize =
            List.range 0 (limit - squareSize)
                |> List.Extra.andThen
                    (\x ->
                        List.range 0 (limit - squareSize)
                            |> List.Extra.andThen
                                (\y ->
                                    [ powerGrid
                                        |> Array.slice y (y + squareSize)
                                        |> Array.map
                                            (Array.slice x (x + squareSize)
                                                >> Array.map (\( xx, yy, value ) -> ( ( xx, yy, squareSize ), value ))
                                            )
                                    ]
                                )
                    )

        scoreSubgrid : Array (Array ( ( Int, Int, Int ), Int )) -> Int
        scoreSubgrid subgrid =
            Array.foldl (\row acc -> acc + Array.foldl (\( ( _, _, _ ), value ) acc_ -> acc_ + value) 0 row) 0 subgrid

        maxSubgrid : Int -> ( String, Int )
        maxSubgrid squareSize =
            squareSize
                |> subgrids
                |> List.Extra.maximumBy scoreSubgrid
                |> Advent.unsafeMaybe
                |> (\s -> ( s |> topLeft |> coordinate, scoreSubgrid s ))
                |> Debug.log (String.fromInt squareSize)

        step : Int -> Int -> String -> String
        step squareSize maxScoreSoFar maxResultString =
            -- we climb to a maximum score and when it starts dropping we bail out
            -- for some reason it just behaves like that
            let
                ( resultString, score ) =
                    maxSubgrid squareSize
            in
            if score > maxScoreSoFar then
                step (squareSize + 1) score resultString

            else
                maxResultString
    in
    step 1 -1 ""



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example"
        "input"
        -1
        -1
     -}
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
6548
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
