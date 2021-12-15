module Year2015.Day14 exposing (..)

import Advent exposing (Test)


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


type alias Input =
    ( Int, List Reindeer )


type alias Output =
    Int


type alias Seconds =
    Int


type alias Distance =
    Int


type alias Name =
    String


type State
    = Running Seconds
    | Resting Seconds


parse : String -> Input
parse input =
    case String.lines input of
        endString :: actualInput ->
            let
                end =
                    Advent.toInt endString
            in
                parseActualInput end actualInput

        _ ->
            Debug.crash "wrong input"


parseActualInput : Int -> List String -> Input
parseActualInput end lines =
    ( end, List.map parseLine lines )


parseLine : String -> Reindeer
parseLine string =
    case String.words string of
        [ name, _, _, speed, _, _, duration, _, _, _, _, _, _, rest, _ ] ->
            let
                durationInt =
                    Advent.toInt duration
            in
                { name = name
                , speed = Advent.toInt speed
                , duration = durationInt
                , rest = Advent.toInt rest
                , distance = 0
                , state = Running durationInt
                , points = 0
                }

        _ ->
            Debug.crash string



--


type alias Reindeer =
    { name : Name
    , speed : Int
    , duration : Seconds
    , rest : Seconds
    , distance : Distance
    , state : State
    , points : Int
    }



--


compute1 : Input -> Output
compute1 ( end, deers ) =
    run end deers
        |> List.map .distance
        |> List.maximum
        |> Advent.unsafeMaybe


run : Seconds -> List Reindeer -> List Reindeer
run seconds deers =
    if seconds == 0 then
        deers
    else
        let
            deersAfterRunning =
                deers
                    |> List.map applySecond

            maxDistance =
                deersAfterRunning
                    |> List.map .distance
                    |> List.maximum
                    |> Advent.unsafeMaybe

            deersAfterPoints =
                deersAfterRunning
                    |> List.map (award maxDistance)
        in
            run (seconds - 1) deersAfterPoints


applySecond : Reindeer -> Reindeer
applySecond deer =
    case deer.state of
        Running secondsLeft ->
            { deer
                | state = newState deer
                , distance = deer.distance + deer.speed
            }

        Resting secondsLeft ->
            { deer | state = newState deer }


award : Int -> Reindeer -> Reindeer
award maxDistance deer =
    if deer.distance == maxDistance then
        { deer | points = deer.points + 1 }
    else
        deer


newState : Reindeer -> State
newState deer =
    case deer.state of
        Running secondsLeft ->
            if secondsLeft == 1 then
                Resting deer.rest
            else
                Running (secondsLeft - 1)

        Resting secondsLeft ->
            if secondsLeft == 1 then
                Running deer.duration
            else
                Resting (secondsLeft - 1)


compute2 : Input -> Output
compute2 ( end, deers ) =
    run end deers
        |> List.map .points
        |> List.maximum
        |> Advent.unsafeMaybe


tests1 : List (Test Input Output)
tests1 =
    [ Test "example"
        """1000
Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."""
        ( 1000
        , [ { name = "Comet"
            , speed = 14
            , duration = 10
            , rest = 127
            , distance = 0
            , state = Running 10
            , points = 0
            }
          , { name = "Dancer"
            , speed = 16
            , duration = 11
            , rest = 162
            , distance = 0
            , state = Running 11
            , points = 0
            }
          ]
        )
        1120
    ]


tests2 : List (Test Input Output)
tests2 =
    []


input : String
input =
    """2503
Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds.
Cupid can fly 22 km/s for 2 seconds, but then must rest for 41 seconds.
Rudolph can fly 11 km/s for 5 seconds, but then must rest for 48 seconds.
Donner can fly 28 km/s for 5 seconds, but then must rest for 134 seconds.
Dasher can fly 4 km/s for 16 seconds, but then must rest for 55 seconds.
Blitzen can fly 14 km/s for 3 seconds, but then must rest for 38 seconds.
Prancer can fly 3 km/s for 21 seconds, but then must rest for 40 seconds.
Comet can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.
Vixen can fly 18 km/s for 5 seconds, but then must rest for 84 seconds."""
