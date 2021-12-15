module Year2015.Day21 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import List.Extra



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    Character


type alias Input2 =
    Character


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


type alias Character =
    { hp : Int
    , dmg : Int
    , armor : Int
    }


parse1 : String -> Input1
parse1 string =
    let
        ints =
            string
                |> String.lines
                |> List.map
                    (\line ->
                        line
                            |> String.replace "Hit Points: " ""
                            |> String.replace "Damage: " ""
                            |> String.replace "Armor: " ""
                            |> String.toInt
                            |> Maybe.withDefault -1
                    )
    in
    case ints of
        [ hp, dmg, armor_ ] ->
            Character hp dmg armor_

        _ ->
            Character -1 -1 -1


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


type alias Item =
    { cost : Int
    , dmg : Int
    , armor : Int
    }


weapons : List Item
weapons =
    [ Item 8 4 0
    , Item 10 5 0
    , Item 25 6 0
    , Item 40 7 0
    , Item 74 8 0
    ]


armor : List Item
armor =
    [ Item 13 0 1
    , Item 31 0 2
    , Item 53 0 3
    , Item 75 0 4
    , Item 102 0 5
    ]


rings : List Item
rings =
    [ Item 25 1 0
    , Item 50 2 0
    , Item 100 3 0
    , Item 20 0 1
    , Item 40 0 2
    , Item 80 0 3
    ]


itemCombinations : List (List Item)
itemCombinations =
    let
        weaponsPossibilities : List (List Item)
        weaponsPossibilities =
            List.map List.singleton weapons

        armorPossibilities : List (List Item)
        armorPossibilities =
            [] :: List.map List.singleton armor

        ringsPossibilities : List (List Item)
        ringsPossibilities =
            []
                :: List.map List.singleton rings
                ++ (List.Extra.uniquePairs rings
                        |> List.map (\( a, b ) -> [ a, b ])
                   )
    in
    List.Extra.cartesianProduct
        [ weaponsPossibilities
        , armorPossibilities
        , ringsPossibilities
        ]
        |> List.map List.concat


possiblePlayers : List ( Int, Character )
possiblePlayers =
    itemCombinations
        |> List.map itemsToPlayer


itemsToPlayer : List Item -> ( Int, Character )
itemsToPlayer items =
    List.foldl
        (\item ( cost, char ) ->
            ( cost + item.cost
            , { char
                | dmg = char.dmg + item.dmg
                , armor = char.armor + item.armor
              }
            )
        )
        ( 0, { hp = 100, dmg = 0, armor = 0 } )
        items


{-| Did player win?
-}
fight : Character -> Character -> Bool
fight player boss =
    fightHelp player boss True


fightHelp : Character -> Character -> Bool -> Bool
fightHelp player boss playersTurn =
    if playersTurn then
        let
            newBoss =
                doDamage player boss
        in
        if newBoss.hp > 0 then
            fightHelp player newBoss False

        else
            True

    else
        let
            newPlayer =
                doDamage boss player
        in
        if newPlayer.hp > 0 then
            fightHelp newPlayer boss True

        else
            False


doDamage : Character -> Character -> Character
doDamage attacker defender =
    { defender | hp = defender.hp - dealtDamage attacker defender }


dealtDamage : Character -> Character -> Int
dealtDamage attacker defender =
    max 1 <|
        (attacker.dmg - defender.armor)


compute1 : Input1 -> Output1
compute1 boss =
    possiblePlayers
        |> List.filter (\( cost, player ) -> fight player boss)
        |> List.sortBy Tuple.first
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault -1


compute2 : Input2 -> Output2
compute2 boss =
    possiblePlayers
        |> List.filter (\( cost, player ) -> not <| fight player boss)
        |> List.sortBy Tuple.first
        |> List.reverse
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault -1



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
Hit Points: 109
Damage: 8
Armor: 2
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
