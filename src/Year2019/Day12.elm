module Year2019.Day12 exposing (..)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Arithmetic
import Array exposing (Array)
import List.Extra
import Maybe.Extra
import Set exposing (Set)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List Position


type alias Input2 =
    List Position


type alias Output1 =
    Int


type alias Output2 =
    Int


type alias Vec3 =
    ( Int, Int, Int )



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> List.map parseLine


parseLine : String -> Vec3
parseLine line =
    -- 5,1,5
    let
        justNumbers =
            line
                |> String.replace "<" ""
                |> String.replace ">" ""
                |> String.replace " " ""
                |> String.replace "x" ""
                |> String.replace "y" ""
                |> String.replace "z" ""
                |> String.replace "=" ""
    in
    case String.split "," justNumbers of
        [ x, y, z ] ->
            ( Advent.unsafeToInt x
            , Advent.unsafeToInt y
            , Advent.unsafeToInt z
            )

        _ ->
            Debug.todo "parse1 - wat"


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


type alias Position =
    Vec3


type alias Velocity =
    Vec3


type alias System a =
    Array ( Position, Velocity, a )


initSystem : a -> List Position -> System a
initSystem initMetadata moonPositions =
    moonPositions
        |> List.map
            (\position ->
                ( position
                , ( 0, 0, 0 )
                , initMetadata
                )
            )
        |> Array.fromList


compute1 : Input1 -> Output1
compute1 input =
    input
        |> initSystem ()
        |> stepTimes 1000
        |> totalEnergy


totalEnergy : System a -> Int
totalEnergy system =
    system
        |> Array.toList
        |> List.map (\( pos, vel, _ ) -> sumAbsTriple pos * sumAbsTriple vel)
        |> List.sum


sumAbsTriple : Vec3 -> Int
sumAbsTriple ( x, y, z ) =
    abs x + abs y + abs z


lcmList : List Int -> Int
lcmList list =
    List.foldl Arithmetic.lcm 1 list


stepTimes : Int -> System a -> System a
stepTimes n system =
    if n == 0 then
        system

    else
        stepTimes (n - 1) (step identity system)


step : (System a -> System a) -> System a -> System a
step updateMeta system =
    system
        |> applyGravity
        |> applyVelocity
        |> updateMeta


{-| Changes velocity by +1, 0, -1
-}
applyGravity : System a -> System a
applyGravity system =
    List.range 0 3
        |> List.Extra.uniquePairs
        |> List.foldl applyGravityForPair system


applyGravityForPair : ( Int, Int ) -> System a -> System a
applyGravityForPair ( i1, i2 ) system =
    let
        pullTogether : Int -> Int -> Int -> Int -> ( Int, Int )
        pullTogether w1 w2 vw1 vw2 =
            case compare w1 w2 of
                LT ->
                    ( vw1 + 1, vw2 - 1 )

                EQ ->
                    ( vw1, vw2 )

                GT ->
                    ( vw1 - 1, vw2 + 1 )

        ( ( x1, y1, z1 ) as pos1, ( vx1, vy1, vz1 ), meta1 ) =
            Array.get i1 system
                |> Advent.unsafeMaybe "applyGravityForPair i1"

        ( ( x2, y2, z2 ) as pos2, ( vx2, vy2, vz2 ), meta2 ) =
            Array.get i2 system
                |> Advent.unsafeMaybe "applyGravityForPair i2"

        ( newVx1, newVx2 ) =
            pullTogether x1 x2 vx1 vx2

        ( newVy1, newVy2 ) =
            pullTogether y1 y2 vy1 vy2

        ( newVz1, newVz2 ) =
            pullTogether z1 z2 vz1 vz2

        newVel1 =
            ( newVx1, newVy1, newVz1 )

        newVel2 =
            ( newVx2, newVy2, newVz2 )
    in
    system
        |> Array.set i1 ( pos1, newVel1, meta1 )
        |> Array.set i2 ( pos2, newVel2, meta2 )


{-| Changes position by velocity
-}
applyVelocity : System a -> System a
applyVelocity system =
    system
        |> Array.map
            (\( pos, vel, meta ) ->
                ( addVec pos vel
                , vel
                , meta
                )
            )


addVec : Vec3 -> Vec3 -> Vec3
addVec ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( x1 + x2
    , y1 + y2
    , z1 + z2
    )


compute2 : Input2 -> Output2
compute2 input =
    input
        |> initSystem
            { x = NotFound Set.empty
            , y = NotFound Set.empty
            , z = NotFound Set.empty
            }
        |> findCycles 0
        |> lcmList


allComponentCycles : System MoonCycles -> Maybe (List Int)
allComponentCycles system =
    system
        |> Array.toList
        |> List.concatMap (\( _, _, { x, y, z } ) -> [ x, y, z ])
        |> List.map getCycleLength
        |> Maybe.Extra.combine


getCycleLength : Cycle -> Maybe Int
getCycleLength cycle =
    case cycle of
        Found c ->
            Just c

        NotFound _ ->
            Nothing


findCycles : Int -> System MoonCycles -> List Int
findCycles n system =
    {-
       let
           str =
               "i,"
                   ++ String.fromInt n
                   ++ ","
                   ++ (List.range 0 3
                           |> List.map
                               (\i ->
                                   Array.get i system
                                       |> Advent.unsafeMaybe "stepTimes"
                                       |> (\( ( x, y, z ), ( vx, vy, vz ), _ ) ->
                                               String.fromInt x
                                                   ++ ","
                                                   ++ String.fromInt vx
                                          )
                               )
                           |> String.join ","
                      )

           _ =
               Debug.log str ()
       in
    -}
    case allComponentCycles system of
        Just cycles ->
            cycles

        Nothing ->
            findCycles (n + 1) (step updateCycles system)


getPositions : (Position -> Int) -> System MoonCycles -> List Int
getPositions fn system =
    system
        |> Array.toList
        |> List.map (\( position, _, _ ) -> fn position)


getVelocities : (Position -> Int) -> System MoonCycles -> List Int
getVelocities fn system =
    system
        |> Array.toList
        |> List.map (\( _, velocity, _ ) -> fn velocity)


updateCycles : System MoonCycles -> System MoonCycles
updateCycles system =
    let
        xs =
            getPositions (\( x, _, _ ) -> x) system

        ys =
            getPositions (\( _, y, _ ) -> y) system

        zs =
            getPositions (\( _, _, z ) -> z) system

        vxs =
            getVelocities (\( x, _, _ ) -> x) system

        vys =
            getVelocities (\( _, y, _ ) -> y) system

        vzs =
            getVelocities (\( _, _, z ) -> z) system
    in
    Array.map (updateCyclesForOneMoon xs ys zs vxs vys vzs) system


updateCycle : List Int -> List Int -> Cycle -> Cycle
updateCycle positions velocities cycle =
    case cycle of
        Found n ->
            Found n

        NotFound set ->
            let
                item =
                    ( positions, velocities )
            in
            if Set.member item set then
                Found (Set.size set)

            else
                NotFound (Set.insert item set)


updateCyclesForOneMoon :
    List Int
    -> List Int
    -> List Int
    -> List Int
    -> List Int
    -> List Int
    -> ( Position, Velocity, MoonCycles )
    -> ( Position, Velocity, MoonCycles )
updateCyclesForOneMoon xs ys zs vxs vys vzs ( pos, vel, cycles ) =
    let
        newCycles =
            { x = updateCycle xs vxs cycles.x
            , y = updateCycle ys vys cycles.y
            , z = updateCycle zs vzs cycles.z
            }
    in
    ( pos, vel, newCycles )


type alias MoonCycles =
    { x : Cycle
    , y : Cycle
    , z : Cycle
    }


type Cycle
    = Found Int
    | NotFound (Set ( List Int, List Int ))



--findCyclesOfMoons : System
-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example"
                """<x=-1, y=0, z=2>
        <x=2, y=-10, z=-7>
        <x=4, y=-8, z=8>
        <x=3, y=5, z=-1>"""
                Nothing -- Just "parsed-input"
                -1
     -}
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    [ {- Test "example 1"
                 """<x=-1, y=0, z=2>
         <x=2, y=-10, z=-7>
         <x=4, y=-8, z=8>
         <x=3, y=5, z=-1>"""
                 Nothing
                 2772
                 ,
      -}
      Test "example 2"
        """<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>"""
        Nothing
        4686774924
    ]



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
<x=5, y=-1, z=5>
<x=0, y=-14, z=2>
<x=16, y=4, z=0>
<x=18, y=1, z=16>
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
