module Year2018.Day10 exposing (Input1, Input2, Output1, Output2, Point, compute1, compute2, input_, main, parse1, parse2, parseLine, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict
import Dict.Extra
import List.Extra
import Set exposing (Set)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List Point


type alias Input2 =
    List Point


type alias Output1 =
    Int


type alias Output2 =
    Int


type alias Point =
    { x : Int
    , y : Int
    , dx : Int
    , dy : Int
    }



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string
        |> String.replace "=" " "
        |> String.replace "<" " "
        |> String.replace ">" " "
        |> String.replace "," " "
        |> String.lines
        |> List.map parseLine


parseLine : String -> Point
parseLine string =
    case String.words string of
        [ _, x, y, _, dx, dy ] ->
            { x = Advent.unsafeToInt x
            , y = Advent.unsafeToInt y
            , dx = Advent.unsafeToInt dx
            , dy = Advent.unsafeToInt dy
            }

        somethingDifferent ->
            Debug.todo ("wrong input 1 " ++ Debug.toString somethingDifferent)


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


score : List Point -> Float
score points =
    let
        rows : List (List Int)
        rows =
            points
                |> Dict.Extra.groupBy .y
                |> Dict.values
                |> List.map (List.map .x >> List.sort)

        columns : List (List Int)
        columns =
            points
                |> Dict.Extra.groupBy .x
                |> Dict.values
                |> List.map (List.map .y >> List.sort)
    in
    (List.map scoreLine rows |> List.sum)
        + (List.map scoreLine columns |> List.sum)


scoreLine : List Int -> Float
scoreLine line =
    toFloat (List.length line) ^ 2 / (toFloat (gaps line) + 1)


gaps : List Int -> Int
gaps line =
    List.Extra.groupsOfWithStep 2 1 line
        |> List.map
            (\pair ->
                case pair of
                    [ a, b ] ->
                        b - a

                    _ ->
                        Debug.todo "wrong input 2"
            )
        |> List.filter (\diff -> diff /= 1)
        |> List.length


fastForward : Int -> List Point -> List Point
fastForward times points =
    if times == 0 then
        points

    else
        fastForward (times - 1) (step points)


step : List Point -> List Point
step points =
    points
        |> List.map
            (\p ->
                { p
                    | x = p.x + p.dx
                    , y = p.y + p.dy
                }
            )


compute1 : Input1 -> Output1
compute1 input =
    go 9990 (fastForward 9990 input) -1


go : Int -> List Point -> Float -> Int
go stepsTaken points maxScore =
    let
        currentScore =
            score points

        nextPoints =
            step points

        nextScore =
            if currentScore > maxScore then
                Debug.log (String.fromInt stepsTaken ++ "\n" ++ show points) currentScore

            else
                maxScore
    in
    go (stepsTaken + 1) nextPoints nextScore


showRow : Int -> Int -> Set Int -> String
showRow minX maxX row =
    List.range minX maxX
        |> List.map
            (\x ->
                if Set.member x row then
                    "#"

                else
                    "."
            )
        |> String.join ""


show : List Point -> String
show points =
    let
        xs : List Int
        xs =
            List.map .x points

        minX : Int
        minX =
            xs
                |> List.minimum
                |> Advent.unsafeMaybe

        maxX : Int
        maxX =
            xs
                |> List.maximum
                |> Advent.unsafeMaybe
    in
    points
        |> Dict.Extra.groupBy .y
        |> Dict.values
        |> List.map (List.map .x >> Set.fromList >> showRow minX maxX)
        |> String.join "\n"


compute2 : Input2 -> Output2
compute2 input =
    -1



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    []


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
position=< 10253, -50152> velocity=<-1,  5>
position=< 20314,  40332> velocity=<-2, -4>
position=<-19881, -30049> velocity=< 2,  3>
position=<-29940,  10166> velocity=< 3, -1>
position=<-40022, -40103> velocity=< 4,  4>
position=< 30403,  20217> velocity=<-3, -2>
position=< 20330,  10172> velocity=<-2, -1>
position=< 30397, -40102> velocity=<-3,  4>
position=< 10305, -50152> velocity=<-1,  5>
position=< 20347,  10167> velocity=<-2, -1>
position=< 30389,  50381> velocity=<-3, -5>
position=< 50485, -40103> velocity=<-5,  4>
position=< -9840,  40325> velocity=< 1, -4>
position=<-29951,  20217> velocity=< 3, -2>
position=< 40441, -30053> velocity=<-4,  3>
position=<-19906,  20217> velocity=< 2, -2>
position=< 40443, -50153> velocity=<-4,  5>
position=< 40446, -19991> velocity=<-4,  2>
position=< 30378,  40325> velocity=<-3, -4>
position=< 20359,  20226> velocity=<-2, -2>
position=<-50072, -30046> velocity=< 5,  3>
position=< 30370,  30271> velocity=<-3, -3>
position=< -9854,  20226> velocity=< 1, -2>
position=<-29948,  10168> velocity=< 3, -1>
position=<-19886,  50388> velocity=< 2, -5>
position=<-50062,  20222> velocity=< 5, -2>
position=<-29921,  30271> velocity=< 3, -3>
position=<-29929,  30276> velocity=< 3, -3>
position=< 30408, -30051> velocity=<-3,  3>
position=< 40422, -40099> velocity=<-4,  4>
position=<-50047,  -9940> velocity=< 5,  1>
position=< 30392, -19995> velocity=<-3,  2>
position=<-39967, -40098> velocity=< 4,  4>
position=< 30371,  30275> velocity=<-3, -3>
position=<-19902, -30045> velocity=< 2,  3>
position=< 30388,  50380> velocity=<-3, -5>
position=< 30352,  10169> velocity=<-3, -1>
position=<-29940,  40333> velocity=< 3, -4>
position=<-50069,  30280> velocity=< 5, -3>
position=<-39978,  10168> velocity=< 4, -1>
position=<-39968,  -9941> velocity=< 4,  1>
position=<-19918, -40102> velocity=< 2,  4>
position=< 10289, -50156> velocity=<-1,  5>
position=< 40419, -50161> velocity=<-4,  5>
position=<-50059, -40106> velocity=< 5,  4>
position=< 10281, -50157> velocity=<-1,  5>
position=< 40454,  50386> velocity=<-4, -5>
position=< 20298, -30044> velocity=<-2,  3>
position=< -9839,  -9945> velocity=< 1,  1>
position=<-19858,  30271> velocity=< 2, -3>
position=<-50040,  40334> velocity=< 5, -4>
position=< -9819,  -9936> velocity=< 1,  1>
position=< 10292, -30049> velocity=<-1,  3>
position=< 30409,  30271> velocity=<-3, -3>
position=< 40443,  30276> velocity=<-4, -3>
position=<-19918, -50157> velocity=< 2,  5>
position=< 10289,  30273> velocity=<-1, -3>
position=<-29914,  50388> velocity=< 3, -5>
position=< 20349,  50383> velocity=<-2, -5>
position=<-39969, -50161> velocity=< 4,  5>
position=< 50484, -40101> velocity=<-5,  4>
position=<-29931,  40330> velocity=< 3, -4>
position=< 50486, -50152> velocity=<-5,  5>
position=< 50508,  40327> velocity=<-5, -4>
position=< 30393, -50160> velocity=<-3,  5>
position=< 50464, -19990> velocity=<-5,  2>
position=< 30392,  50381> velocity=<-3, -5>
position=<-29915, -30044> velocity=< 3,  3>
position=< 50496,  30276> velocity=<-5, -3>
position=< 30370,  30280> velocity=<-3, -3>
position=< 40417,  10163> velocity=<-4, -1>
position=< 10264,  30271> velocity=<-1, -3>
position=< 30364,  40329> velocity=<-3, -4>
position=< -9819, -40099> velocity=< 1,  4>
position=< -9837, -50157> velocity=< 1,  5>
position=< 50496, -40102> velocity=<-5,  4>
position=<-39989,  10172> velocity=< 4, -1>
position=< 20330, -40098> velocity=<-2,  4>
position=<-50048,  20220> velocity=< 5, -2>
position=< 10246,  30275> velocity=<-1, -3>
position=< 10304,  20226> velocity=<-1, -2>
position=<-40023,  10172> velocity=< 4, -1>
position=<-19858, -50157> velocity=< 2,  5>
position=< 30400,  50380> velocity=<-3, -5>
position=<-40006,  50382> velocity=< 4, -5>
position=< 50497,  50382> velocity=<-5, -5>
position=< 30400,  20226> velocity=<-3, -2>
position=< -9861, -19999> velocity=< 1,  2>
position=<-29916,  10165> velocity=< 3, -1>
position=< 40409,  20221> velocity=<-4, -2>
position=<-40015,  30280> velocity=< 4, -3>
position=< 50494, -19999> velocity=<-5,  2>
position=< 10268,  40326> velocity=<-1, -4>
position=< -9821,  20217> velocity=< 1, -2>
position=<-29940, -30047> velocity=< 3,  3>
position=<-19870,  20221> velocity=< 2, -2>
position=< 20346,  20220> velocity=<-2, -2>
position=< 40456, -40103> velocity=<-4,  4>
position=<-19893,  10172> velocity=< 2, -1>
position=<-29960, -19993> velocity=< 3,  2>
position=<-50036,  30276> velocity=< 5, -3>
position=< -9840, -30046> velocity=< 1,  3>
position=<-19906,  20220> velocity=< 2, -2>
position=<-19860,  20217> velocity=< 2, -2>
position=<-29916, -40106> velocity=< 3,  4>
position=<-50061,  -9936> velocity=< 5,  1>
position=<-29972,  30277> velocity=< 3, -3>
position=<-50048, -40104> velocity=< 5,  4>
position=< 20354,  20224> velocity=<-2, -2>
position=<-19866,  50383> velocity=< 2, -5>
position=< -9855,  20226> velocity=< 1, -2>
position=<-40023, -50152> velocity=< 4,  5>
position=< 10245, -50161> velocity=<-1,  5>
position=<-39997, -19999> velocity=< 4,  2>
position=< -9840, -19991> velocity=< 1,  2>
position=< 30394,  50379> velocity=<-3, -5>
position=< 20318, -50161> velocity=<-2,  5>
position=<-50024,  50379> velocity=< 5, -5>
position=<-29972, -50159> velocity=< 3,  5>
position=<-19878,  10168> velocity=< 2, -1>
position=< 30368,  10163> velocity=<-3, -1>
position=< 30371,  20226> velocity=<-3, -2>
position=<-19873,  50383> velocity=< 2, -5>
position=< 20351,  10164> velocity=<-2, -1>
position=< 40433,  -9945> velocity=<-4,  1>
position=< 10248,  50379> velocity=<-1, -5>
position=<-50072,  -9937> velocity=< 5,  1>
position=<-29960,  -9938> velocity=< 3,  1>
position=< 50468, -30045> velocity=<-5,  3>
position=<-50024,  20224> velocity=< 5, -2>
position=< 50484,  -9944> velocity=<-5,  1>
position=< 40451,  10165> velocity=<-4, -1>
position=< 40422, -19992> velocity=<-4,  2>
position=<-19886,  50387> velocity=< 2, -5>
position=< 10293,  -9941> velocity=<-1,  1>
position=< 40418, -40105> velocity=<-4,  4>
position=< 10304,  50383> velocity=<-1, -5>
position=<-29960,  40330> velocity=< 3, -4>
position=<-39983,  50379> velocity=< 4, -5>
position=< 30389, -30047> velocity=<-3,  3>
position=<-40023,  50379> velocity=< 4, -5>
position=<-29916,  40334> velocity=< 3, -4>
position=<-19918,  30280> velocity=< 2, -3>
position=<-29948,  20221> velocity=< 3, -2>
position=<-39989, -30050> velocity=< 4,  3>
position=< 30394, -50161> velocity=<-3,  5>
position=< -9832,  30278> velocity=< 1, -3>
position=<-29923,  40325> velocity=< 3, -4>
position=< 20319, -40105> velocity=<-2,  4>
position=< 30353,  30275> velocity=<-3, -3>
position=< -9804, -50152> velocity=< 1,  5>
position=<-19905,  -9945> velocity=< 2,  1>
position=< -9835, -50152> velocity=< 1,  5>
position=< 40406,  40326> velocity=<-4, -4>
position=< 50508,  10163> velocity=<-5, -1>
position=<-19865,  10165> velocity=< 2, -1>
position=< 10276, -50157> velocity=<-1,  5>
position=<-19870, -19996> velocity=< 2,  2>
position=<-19870,  30280> velocity=< 2, -3>
position=<-39970,  40332> velocity=< 4, -4>
position=< 40462,  50383> velocity=<-4, -5>
position=< 40451,  10170> velocity=<-4, -1>
position=< 40451,  50385> velocity=<-4, -5>
position=<-29924,  30279> velocity=< 3, -3>
position=< 30397,  30280> velocity=<-3, -3>
position=<-40014,  50380> velocity=< 4, -5>
position=<-19858,  50388> velocity=< 2, -5>
position=<-29927,  50387> velocity=< 3, -5>
position=<-29924,  40330> velocity=< 3, -4>
position=<-19918,  -9940> velocity=< 2,  1>
position=< 50484, -19991> velocity=<-5,  2>
position=< 40432, -19999> velocity=<-4,  2>
position=<-50072,  -9937> velocity=< 5,  1>
position=< 30396,  -9944> velocity=<-3,  1>
position=<-19885, -50160> velocity=< 2,  5>
position=< 10273,  -9936> velocity=<-1,  1>
position=< 50484,  -9940> velocity=<-5,  1>
position=<-40002,  20226> velocity=< 4, -2>
position=<-50045, -50161> velocity=< 5,  5>
position=<-29960,  50385> velocity=< 3, -5>
position=<-29972,  20219> velocity=< 3, -2>
position=< -9837,  -9945> velocity=< 1,  1>
position=< 30408, -50161> velocity=<-3,  5>
position=< 20298, -19997> velocity=<-2,  2>
position=<-39985,  10168> velocity=< 4, -1>
position=< 50496,  30272> velocity=<-5, -3>
position=<-29927, -50157> velocity=< 3,  5>
position=<-50076,  50383> velocity=< 5, -5>
position=< 30369,  -9945> velocity=<-3,  1>
position=< 50517,  40334> velocity=<-5, -4>
position=<-29946,  10167> velocity=< 3, -1>
position=< 30368, -50161> velocity=<-3,  5>
position=< 50476,  30280> velocity=<-5, -3>
position=< 30373, -30044> velocity=<-3,  3>
position=< 10244,  50382> velocity=<-1, -5>
position=< 40418,  10167> velocity=<-4, -1>
position=<-50048,  50384> velocity=< 5, -5>
position=< 50487, -40103> velocity=<-5,  4>
position=< -9863, -19990> velocity=< 1,  2>
position=< -9836,  30280> velocity=< 1, -3>
position=<-50028, -19995> velocity=< 5,  2>
position=<-29944, -19990> velocity=< 3,  2>
position=< 20333,  -9945> velocity=<-2,  1>
position=<-50024,  10166> velocity=< 5, -1>
position=<-40002, -40104> velocity=< 4,  4>
position=< 40424, -40102> velocity=<-4,  4>
position=< 10300,  -9944> velocity=<-1,  1>
position=< -9864,  20218> velocity=< 1, -2>
position=<-50043,  20225> velocity=< 5, -2>
position=<-40006, -50158> velocity=< 4,  5>
position=<-50078,  30280> velocity=< 5, -3>
position=< 20318,  40334> velocity=<-2, -4>
position=< 40410,  -9936> velocity=<-4,  1>
position=<-29955, -30047> velocity=< 3,  3>
position=<-50022,  30280> velocity=< 5, -3>
position=<-19894,  10165> velocity=< 2, -1>
position=<-19865,  50382> velocity=< 2, -5>
position=<-29924,  50388> velocity=< 3, -5>
position=<-39981,  50387> velocity=< 4, -5>
position=< 40407,  50388> velocity=<-4, -5>
position=<-29924, -40100> velocity=< 3,  4>
position=< -9816, -30049> velocity=< 1,  3>
position=<-50044,  10168> velocity=< 5, -1>
position=< 50493,  40326> velocity=<-5, -4>
position=< 30402,  -9941> velocity=<-3,  1>
position=<-50043,  30277> velocity=< 5, -3>
position=< 50516,  30280> velocity=<-5, -3>
position=<-29972, -50157> velocity=< 3,  5>
position=< 40454, -30048> velocity=<-4,  3>
position=< -9853,  -9936> velocity=< 1,  1>
position=< 40450, -19998> velocity=<-4,  2>
position=< 30355,  30280> velocity=<-3, -3>
position=<-39975,  30275> velocity=< 4, -3>
position=< 50516,  30277> velocity=<-5, -3>
position=< 40463, -50157> velocity=<-4,  5>
position=< 50497,  30276> velocity=<-5, -3>
position=<-29920,  -9945> velocity=< 3,  1>
position=< 30393,  -9944> velocity=<-3,  1>
position=< 30372, -19996> velocity=<-3,  2>
position=< 20324, -40103> velocity=<-2,  4>
position=< 40459,  30274> velocity=<-4, -3>
position=< 10268,  50387> velocity=<-1, -5>
position=<-50035,  20220> velocity=< 5, -2>
position=<-29927,  50386> velocity=< 3, -5>
position=<-50037, -30048> velocity=< 5,  3>
position=< 20346,  30273> velocity=<-2, -3>
position=< 40463,  10172> velocity=<-4, -1>
position=< 50465, -40098> velocity=<-5,  4>
position=<-19870,  10164> velocity=< 2, -1>
position=< 30354, -50157> velocity=<-3,  5>
position=< 30404, -19995> velocity=<-3,  2>
position=<-50036, -50160> velocity=< 5,  5>
position=< -9811,  -9942> velocity=< 1,  1>
position=<-19897, -40098> velocity=< 2,  4>
position=<-19869, -19999> velocity=< 2,  2>
position=<-19901,  40325> velocity=< 2, -4>
position=<-19890,  30271> velocity=< 2, -3>
position=< 30379,  -9941> velocity=<-3,  1>
position=< 40443,  20223> velocity=<-4, -2>
position=< 40416,  30280> velocity=<-4, -3>
position=< 40462, -50161> velocity=<-4,  5>
position=<-50061,  30271> velocity=< 5, -3>
position=<-19901, -30044> velocity=< 2,  3>
position=< 40407, -50152> velocity=<-4,  5>
position=<-39986,  20223> velocity=< 4, -2>
position=<-29927, -30051> velocity=< 3,  3>
position=< -9824,  50381> velocity=< 1, -5>
position=< 50519,  30280> velocity=<-5, -3>
position=< -9816,  30274> velocity=< 1, -3>
position=<-39989,  -9942> velocity=< 4,  1>
position=<-29916,  10169> velocity=< 3, -1>
position=< 40440,  40330> velocity=<-4, -4>
position=< 20325,  50388> velocity=<-2, -5>
position=< -9839,  20221> velocity=< 1, -2>
position=<-19860,  20217> velocity=< 2, -2>
position=<-40014,  -9937> velocity=< 4,  1>
position=< 50516,  30273> velocity=<-5, -3>
position=<-40014, -50153> velocity=< 4,  5>
position=< 30397, -19992> velocity=<-3,  2>
position=< 20338, -19992> velocity=<-2,  2>
position=< 20310,  30271> velocity=<-2, -3>
position=< 10272,  10163> velocity=<-1, -1>
position=<-39998, -40103> velocity=< 4,  4>
position=< 40454,  40332> velocity=<-4, -4>
position=<-29945,  50379> velocity=< 3, -5>
position=<-40014,  40328> velocity=< 4, -4>
position=< 50508, -40106> velocity=<-5,  4>
position=< 40438,  10170> velocity=<-4, -1>
position=< 10304,  -9945> velocity=<-1,  1>
position=< 20338,  40331> velocity=<-2, -4>
position=<-19907, -19999> velocity=< 2,  2>
position=<-19915, -19999> velocity=< 2,  2>
position=<-29932,  40328> velocity=< 3, -4>
position=< 50520,  40329> velocity=<-5, -4>
position=< 20298,  10171> velocity=<-2, -1>
position=<-40007, -30044> velocity=< 4,  3>
position=< 30377, -30044> velocity=<-3,  3>
position=<-29972,  30276> velocity=< 3, -3>
position=< 10287, -30053> velocity=<-1,  3>
position=< 20330,  20225> velocity=<-2, -2>
position=<-50068,  20224> velocity=< 5, -2>
position=< 30352,  20220> velocity=<-3, -2>
position=< 40458,  50379> velocity=<-4, -5>
position=< 30352,  -9941> velocity=<-3,  1>
position=<-29972,  50386> velocity=< 3, -5>
position=<-50059, -40106> velocity=< 5,  4>
position=<-50076,  10172> velocity=< 5, -1>
position=< 10268,  -9942> velocity=<-1,  1>
position=< 30408,  50383> velocity=<-3, -5>
position=<-40024, -40107> velocity=< 4,  4>
position=< 50495, -50156> velocity=<-5,  5>
position=<-39984,  50384> velocity=< 4, -5>
position=< -9805,  -9945> velocity=< 1,  1>
position=< 50460, -30050> velocity=<-5,  3>
position=< 30364, -30050> velocity=<-3,  3>
position=< 40432, -30053> velocity=<-4,  3>
position=< 10300,  50382> velocity=<-1, -5>
position=< 30364, -30047> velocity=<-3,  3>
position=<-19868, -40107> velocity=< 2,  4>
position=<-39976,  40329> velocity=< 4, -4>
position=<-29960,  40325> velocity=< 3, -4>
position=< 30392, -50156> velocity=<-3,  5>
position=< 10284,  50383> velocity=<-1, -5>
position=< 50509,  -9945> velocity=<-5,  1>
position=<-29972, -30053> velocity=< 3,  3>
position=< 40424, -19994> velocity=<-4,  2>
position=<-40021,  20217> velocity=< 4, -2>
position=<-50045,  50384> velocity=< 5, -5>
position=< 10249, -30053> velocity=<-1,  3>
position=< 20322,  10170> velocity=<-2, -1>
position=< 20322,  10164> velocity=<-2, -1>
position=<-50043, -30045> velocity=< 5,  3>
position=< 30408, -19991> velocity=<-3,  2>
position=<-29928,  50384> velocity=< 3, -5>
position=< 10284,  30279> velocity=<-1, -3>
position=< 40440,  20222> velocity=<-4, -2>
position=< 20335,  -9938> velocity=<-2,  1>
position=<-19857, -19999> velocity=< 2,  2>
position=<-40026,  50386> velocity=< 4, -5>
position=<-40021,  10163> velocity=< 4, -1>
position=< 30371, -50157> velocity=<-3,  5>
position=< 50500, -40104> velocity=<-5,  4>
position=<-29916, -19994> velocity=< 3,  2>
position=<-29924,  20223> velocity=< 3, -2>
position=<-39970,  20218> velocity=< 4, -2>
position=< 40465,  50383> velocity=<-4, -5>
position=<-29940,  40327> velocity=< 3, -4>
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
