module Year2021.Day17 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )



-- 1. TYPES (what is the best representation of the problem?)


type alias Rectangle =
    { x1 : Int
    , x2 : Int
    , y1 : Int
    , y2 : Int
    }


type alias Input1 =
    Rectangle


type alias Input2 =
    Rectangle


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    case
        string
            |> String.dropLeft 15
            |> String.split ", y="
    of
        [ xs, ys ] ->
            let
                ( x1, x2 ) =
                    parseRange xs

                ( y1, y2 ) =
                    parseRange ys
            in
            Rectangle x1 x2 y1 y2

        _ ->
            Debug.todo "parse1"


parseRange : String -> ( Int, Int )
parseRange string =
    case String.split ".." string of
        [ x1, x2 ] ->
            ( x1 |> String.toInt |> Advent.unsafeMaybe "x1"
            , x2 |> String.toInt |> Advent.unsafeMaybe "x2"
            )

        _ ->
            Debug.todo "parseRange"


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


arc : Rectangle -> ( Int, Int ) -> List ( Int, Int )
arc { x1, x2, y1, y2 } velocity =
    let
        go : ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
        go ( vx, vy ) ( x, y ) acc =
            if (x > x1 && x > x2) || (y < y1 && y < y2) then
                List.reverse acc

            else
                let
                    newVelocity =
                        ( towardsZero vx
                        , vy - 1
                        )

                    newPosition =
                        ( x + vx
                        , y + vy
                        )
                in
                go newVelocity newPosition (( x, y ) :: acc)
    in
    go velocity ( 0, 0 ) []


isGood : Rectangle -> ( Int, Int ) -> Bool
isGood rect velocity =
    List.any (hitsRect rect) (arc rect velocity)


hitsRect : Rectangle -> ( Int, Int ) -> Bool
hitsRect { x1, x2, y1, y2 } ( x, y ) =
    x >= x1 && x <= x2 && y >= y1 && y <= y2


towardsZero : Int -> Int
towardsZero n =
    case compare n 0 of
        LT ->
            n + 1

        EQ ->
            0

        GT ->
            n - 1


findVXFromRight : Rectangle -> Int -> Int -> Maybe Int
findVXFromRight rect vy vx =
    if vx < 0 then
        Nothing

    else if isGood rect ( vx, vy ) then
        Just vx

    else
        findVXFromRight rect vy (vx - 1)


findVYFromTop : Rectangle -> Int -> Int
findVYFromTop rect vy =
    if vy < 0 then
        Debug.todo "findVYFromTop"

    else if findVXFromRight rect vy rect.x2 == Nothing then
        findVYFromTop rect (vy - 1)

    else
        vy


compute1 : Input1 -> Output1
compute1 ({ x1, x2, y1, y2 } as rect) =
    let
        bestVY : Int
        bestVY =
            findVYFromTop rect 200

        bestVX : Int
        bestVX =
            findVXFromRight rect bestVY x2
                |> Advent.unsafeMaybe "bestVX"

        bestY : Int
        bestY =
            arc rect ( bestVX, bestVY )
                |> List.map Tuple.second
                |> List.maximum
                |> Advent.unsafeMaybe "bestY"
    in
    bestY


compute2 : Input2 -> Output2
compute2 ({ x1, x2, y1, y2 } as rect) =
    let
        maxVY =
            findVYFromTop rect 200

        go : Int -> Int -> Int
        go vy count =
            if vy < y1 then
                count

            else
                case findVXFromRight rect vy x2 of
                    Nothing ->
                        go (vy - 1) count

                    Just maxGoodVX ->
                        let
                            goInner : ( Int, Int ) -> Int -> Int
                            goInner ( vx_, vy_ ) count_ =
                                if vx_ <= 0 then
                                    count_

                                else if isGood rect ( vx_, vy_ ) then
                                    goInner ( vx_ - 1, vy_ ) (count_ + 1)

                                else
                                    goInner ( vx_ - 1, vy_ ) count_
                        in
                        go (vy - 1) (goInner ( maxGoodVX, vy ) count)
    in
    go maxVY 0



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "example"
        "target area: x=20..30, y=-10..-5"
        Nothing
        45
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    [ Test "example"
        "target area: x=20..30, y=-10..-5"
        Nothing
        112
    ]



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
target area: x=257..286, y=-101..-57
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
