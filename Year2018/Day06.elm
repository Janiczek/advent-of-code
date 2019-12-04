module Year2018.Day06 exposing (Coordinate, Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, parseCoordinate, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)
import Dict.Extra
import List.Extra
import Set exposing (Set)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List Coordinate


type alias Input2 =
    List Coordinate


type alias Output1 =
    Int


type alias Output2 =
    Int


type alias Coordinate =
    ( Int, Int )



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> List.map parseCoordinate


parseCoordinate : String -> Coordinate
parseCoordinate string =
    case String.split ", " string of
        [ x, y ] ->
            ( Advent.unsafeToInt x, Advent.unsafeToInt y )

        _ ->
            Debug.todo "wrong input?"


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    let
        -- left top, right bottom
        ( ( l, t ), ( r, b ) ) =
            workingArea input

        allCoords : List ( Int, Int )
        allCoords =
            List.range l r
                |> List.Extra.andThen
                    (\x ->
                        List.range t b
                            |> List.Extra.andThen (\y -> [ ( x, y ) ])
                    )

        borders : List ( Int, Int )
        borders =
            (List.range l r |> List.map (\x -> ( x, t )))
                ++ (List.range l r |> List.map (\x -> ( x, b )))
                ++ (List.range t b |> List.map (\y -> ( l, y )))
                ++ (List.range t b |> List.map (\y -> ( r, y )))

        bannedCoords : Set ( Int, Int )
        bannedCoords =
            borders
                |> List.map (closest input)
                |> Set.fromList

        counted : Dict ( Int, Int ) Int
        counted =
            allCoords
                |> List.map (closest input)
                |> Dict.Extra.frequencies

        withoutInfinite : Dict ( Int, Int ) Int
        withoutInfinite =
            counted
                |> Dict.filter (\coord _ -> not (Set.member coord bannedCoords))
    in
    withoutInfinite
        |> Dict.values
        |> List.maximum
        |> Advent.unsafeMaybe


manhattan : ( Int, Int ) -> ( Int, Int ) -> Int
manhattan ( ax, ay ) ( bx, by ) =
    abs (ax - bx) + abs (ay - by)


closest : List Coordinate -> ( Int, Int ) -> ( Int, Int )
closest coordinates ( x, y ) =
    coordinates
        |> List.map (\coord -> ( coord, manhattan ( x, y ) coord ))
        |> List.Extra.minimumBy Tuple.second
        |> Advent.unsafeMaybe
        |> Tuple.first


workingArea : List Coordinate -> ( ( Int, Int ), ( Int, Int ) )
workingArea coordinates =
    let
        horizontals =
            List.map Tuple.first coordinates

        verticals =
            List.map Tuple.second coordinates

        minLeft =
            horizontals |> List.minimum |> Advent.unsafeMaybe

        minTop =
            verticals |> List.minimum |> Advent.unsafeMaybe

        maxRight =
            horizontals |> List.maximum |> Advent.unsafeMaybe

        maxBottom =
            verticals |> List.maximum |> Advent.unsafeMaybe
    in
    ( ( minLeft - 1, minTop - 1 ), ( maxRight + 1, maxBottom + 1 ) )


compute2 : Input2 -> Output2
compute2 input =
    let
        limit =
            10000

        -- left top, right bottom
        ( ( l, t ), ( r, b ) ) =
            workingArea input

        allCoords : List ( Int, Int )
        allCoords =
            List.range l r
                |> List.Extra.andThen
                    (\x ->
                        List.range t b
                            |> List.Extra.andThen (\y -> [ ( x, y ) ])
                    )

        isOKHelper : Int -> ( Int, Int ) -> List ( Int, Int ) -> Bool
        isOKHelper distanceSum c1 remainingCoords =
            case remainingCoords of
                [] ->
                    True

                c2 :: rest ->
                    let
                        distance =
                            manhattan c1 c2

                        newSum =
                            distanceSum + distance
                    in
                    if newSum < limit then
                        isOKHelper newSum c1 rest

                    else
                        False

        isOK : ( Int, Int ) -> Bool
        isOK coord =
            isOKHelper 0 coord input
    in
    allCoords
        |> List.filter isOK
        |> List.length



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "example"
        """1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"""
        Nothing
        17
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
268, 273
211, 325
320, 225
320, 207
109, 222
267, 283
119, 70
138, 277
202, 177
251, 233
305, 107
230, 279
243, 137
74, 109
56, 106
258, 97
248, 346
71, 199
332, 215
208, 292
154, 80
74, 256
325, 305
174, 133
148, 51
112, 71
243, 202
136, 237
227, 90
191, 145
345, 133
340, 299
322, 256
86, 323
341, 310
342, 221
50, 172
284, 160
267, 142
244, 153
131, 147
245, 323
42, 241
90, 207
245, 167
335, 106
299, 158
181, 186
349, 286
327, 108
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
