module Year2018.Day15 exposing
    ( Entity(..)
    , Input1
    , Input2
    , Output1
    , Output2
    , compute1
    , compute2
    , getNPCs
    , input_
    , isEnd
    , isNPC
    , main
    , outcomeScore
    , parse1
    , parse2
    , sortByReadingOrder
    , tests1
    , tests2
    )

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Grid exposing (Grid)



-- 1. TYPES (what is the best representation of the problem?)


type alias HP =
    Int


type Entity
    = Wall
    | Empty
    | Goblin HP
    | Elf HP


type alias Input1 =
    Grid Entity


type alias Input2 =
    Grid Entity


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> List.map parseLine
        |> Grid.from2DList (\_ -> Empty)


parseLine : String -> List Entity
parseLine string =
    string
        |> String.toList
        |> List.map parseChar


attackPower : Int
attackPower =
    3


initHp : Int
initHp =
    200


parseChar : Char -> Entity
parseChar char =
    case char of
        '#' ->
            Wall

        '.' ->
            Empty

        'G' ->
            Goblin initHp

        'E' ->
            Elf initHp

        _ ->
            Empty


parse2 : String -> Input2
parse2 string =
    parse1 string


getNPCs : Grid Entity -> List ( ( Int, Int ), Entity )
getNPCs grid =
    grid
        |> Grid.toList
        |> List.filter (\( _, entity ) -> isNPC entity)


getHitpoints : Entity -> Maybe Int
getHitpoints entity =
    case entity of
        Goblin hp ->
            Just hp

        Elf hp ->
            Just hp

        Wall ->
            Nothing

        Empty ->
            Nothing


isGoblin : Entity -> Bool
isGoblin entity =
    case entity of
        Goblin _ ->
            True

        Elf _ ->
            False

        Wall ->
            False

        Empty ->
            False


isNPC : Entity -> Bool
isNPC entity =
    case entity of
        Goblin _ ->
            True

        Elf _ ->
            True

        Wall ->
            False

        Empty ->
            False


sortByReadingOrder : List ( Int, Int ) -> List ( Int, Int )
sortByReadingOrder positions =
    positions
        |> List.sortBy (\( x, y ) -> ( y, x ))


isEnd : List Entity -> Bool
isEnd entities =
    let
        ( goblins, elves ) =
            List.partition isGoblin entities

        sumOfHealth xs =
            List.sum (List.filterMap getHitpoints xs)
    in
    (sumOfHealth goblins <= 0) || (sumOfHealth elves <= 0)


outcomeScore : { fullRounds : Int, hitpoints : List Int } -> Int
outcomeScore { fullRounds, hitpoints } =
    fullRounds * List.sum hitpoints



-- 3. COMPUTE (actually solve the problem)


type alias State =
    { fullRounds : Int
    , grid : Grid Entity
    }


initState : Grid Entity -> State
initState grid =
    { fullRounds = 0
    , grid = grid
    }


compute1 : Input1 -> Output1
compute1 grid =
    let
        finalState : State
        finalState =
            grid
                |> initState
                |> simulateUntilEnd

        finalHitpoints : List Int
        finalHitpoints =
            finalState.grid
                |> getNPCs
                |> List.filterMap (Tuple.second >> getHitpoints)
    in
    outcomeScore
        { fullRounds = finalState.fullRounds
        , hitpoints = finalHitpoints
        }


simulateUntilEnd : State -> State
simulateUntilEnd state =
    if isEnd (List.map Tuple.second (getNPCs state.grid)) then
        state

    else
        Debug.todo "simulate until end - do something"


compute2 : Input2 -> Output2
compute2 input =
    -1



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example"
        "input"
        Nothing -- Just "parsed-input"
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
################################
#####G.####...........##########
######G#.##.#.G......###########
######...##G#.........###...####
########.##...G.......###...####
########..##.....G....#.#......#
######...G##G..................#
#######..#########G............#
#######...#########..G.........#
#######..G.#######..........####
######......G..###..EE......####
######..#.G............E#...####
#######.#.....#####........#####
#######G.....#######...#########
#######.....#########.##########
#######.....#########.##########
#######G....#########.##########
######...EG.#########.##########
######..G...#########.##########
#######......#######..##########
########.E....#####...##########
#...###...G.........E.##########
#....#......G....#..############
#.........G...#......###########
#...#.........#......###########
##.......#######.....###########
#.......########......##########
##...E.E########...E...#########
#####....###########.E.#########
######..#############..#########
#######...######################
################################
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
