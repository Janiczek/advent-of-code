module Year2021.Day18 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import List.Cartesian
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser)



-- 1. TYPES (what is the best representation of the problem?)


type alias Part =
    { value : Int
    , lefts : List Bool
    }


type alias SN =
    List Part


type RecursiveSN
    = Pair RecursiveSN RecursiveSN
    | Regular Int


type alias Input1 =
    List SN


type alias Input2 =
    List SN


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parseLine : String -> SN
parseLine line =
    Parser.run snParser line
        |> Advent.unsafeResult "parseLine"
        |> flatten


snParser : Parser RecursiveSN
snParser =
    Parser.oneOf
        [ pairParser
        , regularParser
        ]


pairParser : Parser RecursiveSN
pairParser =
    Parser.succeed Pair
        |. Parser.token "["
        |= Parser.lazy (\() -> snParser)
        |. Parser.token ","
        |= Parser.lazy (\() -> snParser)
        |. Parser.token "]"


regularParser : Parser RecursiveSN
regularParser =
    Parser.map Regular Parser.int


flatten : RecursiveSN -> SN
flatten sn =
    let
        go lefts sn_ =
            case sn_ of
                Regular n ->
                    [ { value = n, lefts = List.reverse lefts } ]

                Pair x y ->
                    [ go (True :: lefts) x
                    , go (False :: lefts) y
                    ]
                        |> List.concat
    in
    go [] sn


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> List.map parseLine


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


add : SN -> SN -> SN
add x y =
    [ x |> List.map (\n -> { n | lefts = True :: n.lefts })
    , y |> List.map (\n -> { n | lefts = False :: n.lefts })
    ]
        |> List.concat
        |> reduce_


addTo : SN -> SN -> SN
addTo x y =
    add y x


reduce : SN -> SN
reduce sn =
    Nothing
        |> Maybe.orElseLazy (\() -> reduceExplode sn)
        |> Maybe.orElseLazy (\() -> reduceSplit sn)
        |> Maybe.withDefault sn


reduce_ : SN -> SN
reduce_ sn =
    Advent.doUntil (==) reduce sn


reduceExplode : SN -> Maybe SN
reduceExplode sn =
    let
        go sn_ acc =
            case sn_ of
                x :: y :: rest ->
                    let
                        xLeftsLength =
                            List.length x.lefts

                        yLeftsLength =
                            List.length y.lefts

                        butLastX =
                            List.take (xLeftsLength - 1) x.lefts

                        butLastY =
                            List.take (yLeftsLength - 1) y.lefts
                    in
                    if xLeftsLength >= 5 && xLeftsLength == yLeftsLength && butLastX == butLastY then
                        let
                            newLeft =
                                List.head acc
                                    |> Maybe.map (\l -> { l | value = l.value + x.value })

                            newRight =
                                List.head rest
                                    |> Maybe.map (\r -> { r | value = r.value + y.value })

                            newStuff =
                                [ newLeft
                                , Just { value = 0, lefts = butLastX }
                                , newRight
                                ]
                                    |> Maybe.values
                        in
                        Just <|
                            List.reverse
                                (if newLeft == Nothing then
                                    acc

                                 else
                                    List.drop 1 acc
                                )
                                ++ newStuff
                                ++ (if newRight == Nothing then
                                        rest

                                    else
                                        List.drop 1 rest
                                   )

                    else
                        go (y :: rest) (x :: acc)

                _ ->
                    Nothing
    in
    go sn []


reduceSplit : SN -> Maybe SN
reduceSplit sn =
    let
        go sn_ acc =
            case sn_ of
                [] ->
                    Nothing

                x :: xs ->
                    if x.value >= 10 then
                        let
                            half =
                                toFloat x.value / 2

                            newStuff =
                                [ { value = floor half, lefts = x.lefts ++ [ True ] }
                                , { value = ceiling half, lefts = x.lefts ++ [ False ] }
                                ]
                        in
                        Just <| List.reverse acc ++ newStuff ++ xs

                    else
                        go xs (x :: acc)
    in
    go sn []


magnitude : SN -> Int
magnitude sn =
    sn
        |> List.map
            (\n ->
                List.foldr
                    (\left acc ->
                        if left then
                            3 * acc

                        else
                            2 * acc
                    )
                    n.value
                    n.lefts
            )
        |> List.sum


addList : List SN -> SN
addList sns =
    case sns of
        x :: xs ->
            List.foldl addTo x xs

        [] ->
            Debug.todo "compute1"


log : String -> SN -> SN
log label sn =
    let
        _ =
            sn
                |> List.indexedMap (\i n -> Debug.log (label ++ " " ++ String.fromInt i) n)
    in
    sn


compute1 : Input1 -> Output1
compute1 input =
    addList input
        |> magnitude


compute2 : Input2 -> Output2
compute2 input =
    List.Cartesian.map2
        (\a b -> addList [ a, b ] |> magnitude)
        input
        input
        |> List.maximum
        |> Advent.unsafeMaybe "compute2"



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "example"
        """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"""
        Nothing
        4140
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    [ Test "example"
        """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"""
        Nothing
        3993
    ]



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
[[[[4,6],4],[1,7]],[[[1,6],[8,4]],[1,1]]]
[[[[8,5],[9,2]],1],[[2,5],[[9,4],[5,9]]]]
[[[[7,3],0],[8,9]],6]
[[6,[[7,2],[6,2]]],[[[9,8],9],[9,6]]]
[2,[[[9,2],6],[[5,3],[6,7]]]]
[[[5,[9,6]],0],[[[2,8],[7,0]],[7,[4,4]]]]
[[[[5,0],2],[0,1]],4]
[2,[8,8]]
[[[[2,5],[6,8]],[[9,8],4]],[[[2,3],[5,8]],[9,5]]]
[[[[0,7],[9,4]],[[1,0],9]],[[[8,8],[7,2]],[3,[6,5]]]]
[[[[3,2],8],1],[[4,[3,4]],[[6,5],[0,6]]]]
[[[7,8],8],[0,[5,2]]]
[[3,[3,3]],[[[6,9],[1,1]],[6,[2,9]]]]
[[[[9,7],[6,8]],4],[[[8,2],[2,9]],[8,[6,2]]]]
[[[[7,3],2],[9,6]],[[[1,7],[0,0]],[4,9]]]
[[8,[7,[1,0]]],7]
[[[7,[5,1]],0],[[8,[5,3]],4]]
[1,[[[2,6],2],[1,[6,0]]]]
[[[5,8],[[9,1],1]],[[3,[5,0]],5]]
[[[[1,5],[4,9]],8],[[7,0],6]]
[9,[[0,[1,0]],6]]
[[[[6,8],6],9],[[7,3],2]]
[[9,[[8,7],4]],[[[4,0],[9,0]],[8,1]]]
[[[2,[4,4]],[7,[0,1]]],[8,[[8,6],[4,0]]]]
[0,9]
[[[[1,8],[7,4]],[[5,0],[6,1]]],[5,7]]
[[[[8,2],[9,2]],[8,[8,4]]],[0,4]]
[[[[0,7],[5,8]],3],6]
[[[7,[3,4]],[3,[1,5]]],2]
[[[1,[4,2]],5],[[1,2],1]]
[[[[8,2],[0,9]],1],[[[9,0],[3,5]],[8,[8,0]]]]
[[[0,5],[1,[3,3]]],[[[1,0],[5,2]],[7,5]]]
[[[4,[7,3]],[0,9]],[[2,0],8]]
[[[[2,2],8],[7,1]],5]
[[1,[[3,8],7]],[[7,[5,8]],[4,[1,7]]]]
[[[[2,7],4],[8,[9,1]]],[[5,2],[4,3]]]
[[2,[7,2]],[[8,[0,8]],[0,[4,2]]]]
[[6,[6,[7,4]]],[[7,[2,0]],[[8,2],8]]]
[[[7,[1,7]],[[4,1],4]],[1,[4,6]]]
[1,[[1,0],[[0,3],[6,9]]]]
[[[[8,6],0],[[2,8],[3,0]]],[[[8,2],7],[[3,0],5]]]
[[[[2,8],4],[2,[0,7]]],[[3,[1,2]],[[8,0],[4,2]]]]
[1,8]
[[5,6],6]
[[[[1,0],[3,6]],[[4,0],1]],[0,7]]
[[[5,[9,6]],[7,[1,2]]],2]
[[[6,4],[[5,6],[1,8]]],[[[9,0],[7,7]],[[5,8],[6,8]]]]
[8,5]
[5,[[[6,8],8],0]]
[[[[5,7],[0,0]],[6,[0,0]]],[[[5,5],3],[[1,1],[3,4]]]]
[[[4,0],[[8,6],2]],[[3,[3,1]],[[2,8],[7,2]]]]
[[[8,7],[[5,5],[5,3]]],4]
[[[[5,4],1],[3,4]],[3,5]]
[[[6,5],[[6,3],6]],4]
[[[[2,2],[7,1]],[6,6]],[[8,[8,7]],[[1,6],[3,0]]]]
[[4,[[5,0],[7,4]]],[3,1]]
[[[3,[5,8]],5],[1,[[9,6],3]]]
[[0,[[3,0],[8,7]]],[[1,3],3]]
[5,[[3,[3,3]],[3,6]]]
[[[[7,3],8],3],[2,[[9,8],2]]]
[[[2,4],[[1,2],5]],[[[1,2],[6,0]],3]]
[[9,[[1,1],[1,7]]],[1,[2,[9,1]]]]
[[[5,[0,0]],5],[6,[0,1]]]
[[3,[[6,5],7]],[[7,8],3]]
[[5,[2,6]],8]
[[6,[0,[3,0]]],[1,2]]
[3,[[[3,7],2],[[4,0],6]]]
[[[8,[2,7]],[4,1]],[[2,[4,2]],3]]
[[3,2],[[[8,8],[8,6]],[[5,3],1]]]
[1,[2,[[3,2],[2,9]]]]
[8,[[9,1],[[8,4],[9,9]]]]
[[[4,[4,6]],[1,8]],[[7,7],[[7,4],3]]]
[[[8,2],[[9,7],[0,8]]],[[4,4],[[6,1],5]]]
[[[3,[6,6]],[[8,6],[3,7]]],[[7,9],[[5,3],8]]]
[[[8,9],[8,6]],[[[3,3],[2,9]],[[6,6],9]]]
[8,[[[3,0],5],2]]
[[[[1,3],1],[[1,9],4]],[7,[3,1]]]
[[[[9,3],3],[[6,8],7]],[[[2,0],3],[8,[3,6]]]]
[[[[7,1],[8,1]],[[4,6],[5,9]]],[[[4,5],3],5]]
[6,[[3,[0,0]],[6,6]]]
[[[[8,8],[7,6]],3],[[[7,7],[1,1]],[[1,8],[1,4]]]]
[[9,[8,[3,4]]],[[6,2],[1,5]]]
[[5,[3,3]],[5,[0,5]]]
[[[[8,9],5],[1,9]],[[5,[2,8]],[[6,4],[9,4]]]]
[2,6]
[[[[1,4],8],5],[5,[0,[1,7]]]]
[[[[1,0],[9,9]],[0,9]],[[[5,4],[1,6]],[9,[6,7]]]]
[[[7,1],5],[[3,2],5]]
[[9,[[8,8],[7,0]]],[5,[3,[1,3]]]]
[[[[5,2],[7,5]],[4,[6,7]]],[[[8,1],6],[2,[6,6]]]]
[[[5,7],[6,[8,2]]],[8,2]]
[[[[5,7],8],[[9,8],2]],[[2,8],[[7,6],3]]]
[[1,[[1,6],1]],[0,[[5,9],[9,1]]]]
[[[[1,4],[5,0]],[[5,5],[9,3]]],[[6,4],[4,[4,6]]]]
[7,[[5,[4,8]],[[5,9],2]]]
[[[[2,9],[1,8]],[4,2]],0]
[[5,[[0,9],[3,7]]],[2,[6,[4,8]]]]
[[0,[5,5]],0]
[[[5,0],[[0,5],8]],[6,[[8,7],[6,5]]]]
[[[5,[8,2]],[8,4]],[[6,2],[8,[7,0]]]]
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
