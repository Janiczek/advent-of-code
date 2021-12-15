module Year2015.Day15 exposing (..)

import Advent exposing (Test)
import List.Extra
import String.Extra


main : Program Never ( Output, Output ) Never
main =
    Advent.program
        { input = input
        , parse1 = parse
        , parse2 = parse
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }


type alias Ingredient =
    { capacity : Int
    , durability : Int
    , flavor : Int
    , texture : Int
    , calories : Int
    }


type alias Input =
    List Ingredient


type alias Output =
    Int


parse : String -> Input
parse input =
    input
        |> String.lines
        |> List.map parseLine


parseLine : String -> Ingredient
parseLine line =
    case
        line
            |> String.Extra.replace "," ""
            |> String.words
    of
        [ _, _, capacity, _, durability, _, flavor, _, texture, _, calories ] ->
            { capacity = Advent.toInt capacity
            , durability = Advent.toInt durability
            , flavor = Advent.toInt flavor
            , texture = Advent.toInt texture
            , calories = Advent.toInt calories
            }

        _ ->
            Debug.crash "wrong input"


coefficientLists : Int -> Int -> List (List Int)
coefficientLists maxLength sum =
    coefficientListsHelper maxLength sum (List.range 0 sum) [] []


coefficientListsHelper :
    Int
    -> Int
    -> List Int
    -> List Int
    -> List (List Int)
    -> List (List Int)
coefficientListsHelper maxLength targetSum allowedNumbers currentList lists =
    let
        currentSum =
            List.sum currentList

        len =
            List.length currentList

        newLists =
            if currentSum == targetSum then
                currentList :: lists
            else
                lists
    in
        if len < maxLength then
            newLists
                ++ (allowedNumbers
                        |> List.filter (\x -> x + currentSum <= targetSum)
                        |> List.concatMap
                            (\x ->
                                coefficientListsHelper
                                    maxLength
                                    targetSum
                                    allowedNumbers
                                    (x :: currentList)
                                    newLists
                            )
                   )
        else if len == maxLength then
            newLists
        else
            []


compute1 : Input -> Output
compute1 input =
    coefficientLists (List.length input) 100
        |> List.map (\x -> ( x, evaluate input x ))
        |> List.sortBy (\( x, score ) -> score)
        |> List.reverse
        |> List.head
        |> Advent.unsafeMaybe
        |> Tuple.second


evaluate : List Ingredient -> List Int -> Int
evaluate ingredients tries =
    List.map2
        (\amount { capacity, durability, flavor, texture, calories } ->
            { capacity = amount * capacity
            , durability = amount * durability
            , flavor = amount * flavor
            , texture = amount * texture
            , calories = amount * calories
            }
        )
        tries
        ingredients
        |> List.Extra.foldl1
            (\i1 i2 ->
                { capacity = i1.capacity + i2.capacity
                , durability = i1.durability + i2.durability
                , flavor = i1.flavor + i2.flavor
                , texture = i1.texture + i2.texture
                , calories = i1.calories + i2.calories
                }
            )
        |> Advent.unsafeMaybe
        |> clampToZero
        |> product


clampToZero : Ingredient -> Ingredient
clampToZero { capacity, durability, flavor, texture, calories } =
    { capacity = max 0 capacity
    , durability = max 0 durability
    , flavor = max 0 flavor
    , texture = max 0 texture
    , calories = max 0 calories
    }


product : Ingredient -> Int
product ingredient =
    ingredient.capacity
        * ingredient.durability
        * ingredient.flavor
        * ingredient.texture


compute2 : Input -> Output
compute2 input =
    coefficientLists (List.length input) 100
        |> List.filter (\x -> calories input x == 500)
        |> List.map (\x -> ( x, evaluate input x ))
        |> List.sortBy (\( x, score ) -> score)
        |> List.reverse
        |> List.head
        |> Advent.unsafeMaybe
        |> Tuple.second


calories : List Ingredient -> List Int -> Int
calories ingredients tries =
    List.map2
        (\amount { capacity, durability, flavor, texture, calories } ->
            { capacity = amount * capacity
            , durability = amount * durability
            , flavor = amount * flavor
            , texture = amount * texture
            , calories = amount * calories
            }
        )
        tries
        ingredients
        |> List.Extra.foldl1
            (\i1 i2 ->
                { capacity = i1.capacity + i2.capacity
                , durability = i1.durability + i2.durability
                , flavor = i1.flavor + i2.flavor
                , texture = i1.texture + i2.texture
                , calories = i1.calories + i2.calories
                }
            )
        |> Advent.unsafeMaybe
        |> clampToZero
        |> .calories


tests1 : List (Test Input Output)
tests1 =
    [ Test "example"
        """Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"""
        [ Ingredient -1 -2 6 3 8
        , Ingredient 2 3 -2 -1 3
        ]
        62842880
    ]


tests2 : List (Test Input Output)
tests2 =
    []


input : String
input =
    """Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5
Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8
Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6
Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1"""
