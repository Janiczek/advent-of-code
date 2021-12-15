module Year2017.Day11 exposing (..)

import Advent exposing (Test)
import EveryDict as Dict exposing (EveryDict)


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


type Direction
    = N
    | NE
    | SE
    | S
    | SW
    | NW


type alias Input =
    List Direction


type alias Output =
    Int


parse : String -> Input
parse input =
    input
        |> String.split ","
        |> List.map parseDirection


parseDirection : String -> Direction
parseDirection string =
    case string of
        "n" ->
            N

        "ne" ->
            NE

        "se" ->
            SE

        "s" ->
            S

        "sw" ->
            SW

        "nw" ->
            NW

        _ ->
            Debug.crash "wrong input"


compute1 : Input -> Output
compute1 input =
    input
        |> frequencies
        |> cancelOut
        |> sum


cancelOut : Counts -> Counts
cancelOut directions =
    directions
        |> cancelSwSe
        |> cancelNwNe
        |> cancelNwSe
        |> cancelSwNe
        |> cancelNSe
        |> cancelNSw
        |> cancelSNe
        |> cancelSNw
        |> cancelNS
        |> cancelNSw


cancelSwSe : Counts -> Counts
cancelSwSe ({ sw, se, s } as directions) =
    let
        cancelled =
            min sw se

        newS =
            s + cancelled

        newSw =
            sw - cancelled

        newSe =
            se - cancelled
    in
        { directions
            | sw = newSw
            , se = newSe
            , s = newS
        }


cancelNwNe : Counts -> Counts
cancelNwNe ({ nw, ne, n } as directions) =
    let
        cancelled =
            min nw ne

        newN =
            n + cancelled

        newNw =
            nw - cancelled

        newNe =
            ne - cancelled
    in
        { directions
            | nw = newNw
            , ne = newNe
            , n = newN
        }


cancelNwSe : Counts -> Counts
cancelNwSe ({ nw, se } as directions) =
    let
        cancelled =
            min nw se

        newNw =
            nw - cancelled

        newSe =
            se - cancelled
    in
        { directions
            | nw = newNw
            , se = newSe
        }


cancelSwNe : Counts -> Counts
cancelSwNe ({ sw, ne } as directions) =
    let
        cancelled =
            min sw ne

        newSw =
            sw - cancelled

        newNe =
            ne - cancelled
    in
        { directions
            | sw = newSw
            , ne = newNe
        }


cancelNS : Counts -> Counts
cancelNS ({ n, s } as directions) =
    let
        cancelled =
            min n s

        newN =
            n - cancelled

        newS =
            s - cancelled
    in
        { directions
            | n = newN
            , s = newS
        }


cancelNSe : Counts -> Counts
cancelNSe ({ n, se, ne } as directions) =
    let
        cancelled =
            min n se

        newNe =
            ne + cancelled

        newN =
            n - cancelled

        newSe =
            se - cancelled
    in
        { directions
            | n = newN
            , se = newSe
            , ne = newNe
        }


cancelSNe : Counts -> Counts
cancelSNe ({ s, ne, se } as directions) =
    let
        cancelled =
            min s ne

        newSe =
            se + cancelled

        newS =
            s - cancelled

        newNe =
            ne - cancelled
    in
        { directions
            | s = newS
            , ne = newNe
            , se = newSe
        }


cancelNSw : Counts -> Counts
cancelNSw ({ n, sw, nw } as directions) =
    let
        cancelled =
            min n sw

        newNw =
            nw + cancelled

        newN =
            n - cancelled

        newSw =
            sw - cancelled
    in
        { directions
            | n = newN
            , sw = newSw
            , nw = newNw
        }


cancelSNw : Counts -> Counts
cancelSNw ({ s, nw, sw } as directions) =
    let
        cancelled =
            min s nw

        newSw =
            sw + cancelled

        newS =
            s - cancelled

        newNw =
            nw - cancelled
    in
        { directions
            | s = newS
            , nw = newNw
            , sw = newSw
        }


compute2 : Input -> Output
compute2 input =
    input
        |> List.scanl step ( 0, 0, 0 )
        |> List.map distanceFromOrigin
        |> List.maximum
        |> Maybe.withDefault 0


step : Direction -> ( Int, Int, Int ) -> ( Int, Int, Int )
step direction ( x, y, z ) =
    case direction of
        N ->
            ( x, y + 1, z - 1 )

        S ->
            ( x, y - 1, z + 1 )

        NE ->
            ( x + 1, y, z - 1 )

        SE ->
            ( x + 1, y - 1, z )

        NW ->
            ( x - 1, y + 1, z )

        SW ->
            ( x - 1, y, z + 1 )


distanceFromOrigin : ( Int, Int, Int ) -> Int
distanceFromOrigin ( x, y, z ) =
    let
        ax =
            abs x

        ay =
            abs y

        az =
            abs z

        list =
            [ ax, ay, az ]
    in
        List.maximum [ ax, ay, az ]
            |> Maybe.withDefault 0


tests1 : List (Test Input Output)
tests1 =
    [ Test "example 1" "ne,ne,ne" [ NE, NE, NE ] 3
    , Test "example 2" "ne,ne,sw,sw" [ NE, NE, SW, SW ] 0
    , Test "example 3" "ne,ne,s,s" [ NE, NE, S, S ] 2
    , Test "example 4" "se,sw,se,sw,sw" [ SE, SW, SE, SW, SW ] 3
    ]


tests2 : List (Test Input Output)
tests2 =
    []


group : List a -> EveryDict a (List a)
group list =
    List.foldr
        (\x acc ->
            Dict.update
                (identity x)
                (Maybe.map ((::) x)
                    >> Maybe.withDefault [ x ]
                    >> Just
                )
                acc
        )
        Dict.empty
        list


type alias Counts =
    { n : Int
    , nw : Int
    , ne : Int
    , s : Int
    , se : Int
    , sw : Int
    }


frequencies : List Direction -> Counts
frequencies list =
    let
        directions =
            list
                |> group
                |> Dict.map (\k v -> List.length v)

        n =
            get N directions

        nw =
            get NW directions

        ne =
            get NE directions

        s =
            get S directions

        sw =
            get SW directions

        se =
            get SE directions
    in
        { n = n
        , ne = ne
        , nw = nw
        , s = s
        , se = se
        , sw = sw
        }


get : Direction -> EveryDict Direction Int -> Int
get direction dict =
    dict
        |> Dict.get direction
        |> Maybe.withDefault 0


sum : Counts -> Int
sum { ne, nw, se, sw, s, n } =
    ne + nw + se + sw + s + n


input : String
input =
    "s,se,ne,ne,ne,ne,ne,ne,ne,n,sw,sw,nw,sw,n,nw,nw,sw,nw,nw,nw,nw,sw,nw,nw,nw,nw,s,nw,nw,sw,nw,sw,sw,sw,nw,sw,sw,sw,sw,sw,sw,sw,s,sw,sw,sw,sw,se,sw,s,s,sw,s,ne,nw,nw,s,n,s,sw,s,sw,nw,se,s,s,s,s,s,nw,s,nw,s,s,se,se,se,se,se,se,s,se,s,s,se,ne,se,se,se,se,se,ne,ne,se,se,se,se,se,ne,se,se,se,sw,sw,se,ne,s,ne,se,sw,n,sw,ne,ne,se,se,ne,se,ne,se,se,se,se,sw,ne,se,se,ne,se,sw,se,ne,se,ne,s,ne,ne,ne,ne,s,se,ne,ne,ne,s,ne,nw,ne,s,ne,ne,s,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,n,ne,ne,ne,n,n,se,ne,ne,ne,ne,n,ne,ne,n,nw,ne,sw,ne,ne,n,n,ne,sw,n,ne,n,s,n,n,ne,sw,ne,ne,s,n,n,n,n,se,ne,ne,n,n,n,ne,n,n,n,n,n,n,n,nw,n,s,n,se,n,n,n,nw,n,n,n,n,nw,n,n,n,n,se,sw,n,nw,n,nw,n,ne,sw,n,ne,n,s,n,n,n,nw,nw,n,n,n,n,n,n,n,n,nw,nw,nw,n,nw,ne,n,nw,n,nw,nw,nw,nw,nw,nw,n,nw,s,nw,n,nw,sw,nw,ne,nw,sw,nw,nw,n,nw,nw,nw,sw,nw,nw,nw,n,sw,nw,nw,nw,nw,nw,nw,nw,nw,nw,n,nw,n,nw,se,nw,nw,nw,nw,nw,se,nw,nw,nw,nw,nw,s,s,nw,nw,nw,sw,nw,nw,nw,nw,n,nw,se,nw,nw,sw,nw,sw,s,sw,nw,s,nw,sw,nw,nw,nw,n,nw,n,nw,sw,n,nw,nw,nw,sw,nw,n,s,nw,nw,nw,se,s,se,nw,sw,s,nw,sw,nw,sw,se,ne,nw,nw,se,nw,nw,sw,sw,nw,nw,sw,s,nw,s,nw,nw,nw,sw,nw,nw,sw,nw,nw,sw,nw,sw,s,nw,nw,sw,sw,se,sw,nw,nw,sw,n,nw,sw,sw,sw,sw,sw,sw,s,nw,sw,sw,sw,s,nw,nw,sw,nw,sw,nw,sw,s,sw,sw,s,sw,sw,ne,s,sw,sw,se,sw,sw,sw,sw,sw,sw,se,nw,sw,sw,sw,sw,nw,nw,sw,nw,sw,sw,sw,sw,sw,sw,s,sw,se,sw,se,nw,sw,sw,sw,sw,sw,sw,s,ne,nw,sw,s,sw,nw,sw,s,s,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,nw,sw,sw,n,sw,sw,sw,sw,sw,sw,ne,s,ne,sw,sw,s,sw,sw,sw,sw,sw,ne,sw,sw,s,nw,nw,sw,se,s,sw,n,n,sw,sw,se,sw,sw,sw,sw,sw,sw,se,n,ne,sw,sw,ne,s,s,s,sw,s,sw,s,sw,n,sw,s,s,se,s,sw,sw,se,s,sw,n,n,sw,sw,s,sw,s,s,sw,nw,s,sw,s,s,ne,s,s,n,s,sw,s,s,s,s,sw,s,ne,n,s,s,nw,ne,sw,s,s,n,s,s,n,s,sw,s,s,sw,s,s,s,ne,s,sw,s,sw,ne,s,sw,sw,s,ne,n,s,sw,s,s,nw,sw,s,s,sw,s,s,ne,s,s,s,s,s,s,s,s,s,sw,se,s,se,s,se,ne,se,s,s,s,sw,sw,s,s,sw,s,s,se,s,s,s,s,s,s,s,s,s,s,n,s,s,s,nw,s,s,s,s,ne,s,se,se,ne,s,s,s,ne,s,s,s,s,s,s,s,n,se,s,s,se,se,s,n,s,s,n,sw,se,n,s,s,ne,s,se,s,s,se,s,s,s,s,se,s,s,se,se,se,nw,s,s,s,s,se,n,s,se,s,n,s,s,s,s,s,s,s,se,se,s,se,n,se,sw,se,s,se,n,se,s,n,nw,sw,s,sw,se,sw,se,s,ne,s,s,s,se,sw,se,s,s,s,sw,nw,ne,s,s,s,nw,se,s,sw,sw,se,s,se,se,se,se,s,se,s,s,n,sw,nw,se,se,ne,s,se,s,s,nw,s,s,se,se,se,s,n,se,se,se,s,s,se,ne,se,s,ne,nw,nw,s,n,n,nw,se,s,s,se,s,se,se,se,s,s,s,se,s,se,s,ne,s,n,s,sw,ne,se,s,se,se,se,se,se,s,se,se,se,s,n,se,ne,se,se,se,se,s,se,s,s,se,nw,se,s,s,se,se,n,se,se,se,se,se,se,se,s,se,se,se,nw,se,se,s,se,se,s,se,se,se,se,s,s,n,sw,se,ne,se,se,se,s,sw,se,ne,se,se,se,se,se,s,se,nw,se,se,se,se,se,n,se,s,se,n,se,se,se,se,se,se,nw,s,se,se,s,se,se,se,se,n,nw,se,se,se,sw,se,sw,ne,se,se,se,se,se,se,se,se,se,se,se,se,se,se,ne,nw,se,se,ne,se,se,se,nw,se,se,sw,se,se,se,sw,se,se,se,se,se,se,se,se,nw,se,se,se,se,se,se,se,se,se,se,ne,se,sw,se,se,ne,se,se,s,se,se,ne,ne,se,se,se,se,se,se,se,se,s,se,nw,sw,se,se,se,nw,se,ne,s,se,ne,nw,se,nw,se,se,se,se,ne,sw,ne,se,se,se,se,sw,se,ne,se,s,ne,nw,ne,ne,se,n,se,se,se,sw,ne,se,se,nw,sw,se,se,se,se,ne,s,nw,n,ne,ne,ne,se,se,nw,ne,se,ne,se,se,se,nw,ne,ne,se,se,se,nw,ne,ne,se,nw,ne,se,s,ne,ne,ne,se,se,ne,n,se,se,n,se,se,se,se,ne,sw,se,ne,ne,se,se,ne,se,se,sw,ne,sw,ne,se,s,se,n,ne,se,nw,se,se,se,ne,se,sw,ne,sw,se,ne,se,se,se,ne,nw,ne,ne,ne,ne,ne,ne,se,se,se,se,n,nw,sw,se,ne,se,n,ne,s,se,n,ne,se,se,ne,sw,ne,se,ne,s,ne,ne,se,se,ne,ne,n,ne,ne,ne,s,ne,ne,ne,se,sw,ne,ne,ne,se,ne,s,se,nw,nw,ne,se,ne,ne,n,se,se,s,ne,se,ne,ne,se,ne,ne,nw,ne,n,se,ne,se,se,ne,ne,ne,s,ne,s,ne,ne,ne,se,ne,n,nw,sw,se,ne,ne,nw,s,se,ne,se,ne,ne,ne,ne,ne,n,ne,sw,ne,s,se,ne,ne,se,ne,se,se,ne,ne,se,sw,n,ne,s,sw,s,ne,ne,ne,ne,ne,sw,ne,se,se,sw,ne,ne,ne,ne,ne,ne,ne,se,ne,ne,se,se,ne,ne,nw,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,se,ne,ne,sw,ne,se,sw,s,ne,ne,ne,s,s,ne,ne,n,ne,se,ne,ne,n,ne,ne,ne,ne,sw,ne,ne,ne,sw,sw,ne,ne,ne,ne,ne,sw,ne,ne,ne,ne,se,ne,ne,ne,ne,ne,ne,se,ne,sw,nw,ne,ne,ne,n,ne,ne,ne,ne,nw,nw,sw,n,ne,ne,nw,ne,ne,ne,ne,nw,ne,nw,ne,ne,ne,n,ne,ne,ne,sw,ne,n,sw,nw,ne,ne,n,ne,ne,s,ne,ne,ne,ne,ne,ne,ne,ne,n,n,ne,ne,se,nw,ne,n,sw,ne,ne,ne,ne,ne,ne,n,ne,ne,ne,ne,ne,nw,ne,ne,ne,se,s,ne,n,ne,ne,ne,ne,se,n,n,ne,ne,ne,n,s,n,ne,n,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,se,ne,sw,n,nw,s,ne,nw,s,ne,n,ne,ne,ne,ne,n,se,ne,ne,ne,ne,ne,se,n,nw,ne,ne,n,ne,ne,n,ne,n,ne,ne,ne,n,ne,se,ne,ne,ne,sw,ne,ne,ne,ne,n,ne,ne,ne,n,n,ne,ne,ne,se,n,ne,ne,ne,ne,ne,ne,ne,n,ne,ne,s,ne,ne,ne,ne,ne,se,se,nw,n,n,nw,ne,ne,n,ne,ne,ne,s,ne,ne,ne,n,nw,s,nw,n,n,nw,n,se,ne,ne,ne,nw,ne,ne,ne,ne,ne,sw,n,ne,ne,ne,ne,n,ne,n,se,ne,n,n,ne,sw,ne,n,ne,n,ne,n,ne,ne,n,ne,n,ne,n,ne,nw,ne,ne,n,ne,sw,sw,n,ne,n,ne,ne,ne,ne,ne,ne,ne,ne,nw,nw,s,ne,n,ne,nw,n,ne,ne,n,n,sw,nw,n,ne,ne,n,n,ne,ne,ne,ne,ne,n,nw,ne,n,ne,ne,ne,ne,n,n,s,ne,se,s,ne,sw,ne,n,sw,n,ne,n,n,n,nw,n,nw,n,se,ne,ne,ne,n,ne,n,ne,sw,ne,n,ne,n,n,sw,ne,nw,n,ne,ne,n,ne,n,n,se,s,n,n,n,n,ne,sw,ne,n,se,se,se,n,s,n,n,n,n,n,ne,n,ne,n,sw,ne,n,ne,n,ne,n,ne,n,sw,s,n,n,ne,ne,n,n,n,s,se,n,se,n,ne,n,n,n,n,n,se,sw,ne,n,n,n,n,n,n,se,n,s,nw,sw,ne,n,n,ne,n,n,n,n,n,ne,ne,n,n,ne,n,ne,nw,n,ne,ne,ne,ne,n,n,nw,nw,nw,n,n,n,nw,ne,ne,n,nw,nw,ne,se,se,n,sw,ne,ne,ne,n,n,n,ne,nw,nw,n,nw,n,n,n,nw,n,n,nw,sw,n,se,n,s,n,n,n,ne,n,n,n,ne,n,n,n,n,n,nw,n,n,n,ne,n,n,n,se,n,n,n,se,n,n,n,n,n,n,s,n,n,n,n,ne,n,n,n,n,n,n,n,n,sw,n,ne,s,ne,n,n,sw,n,n,n,se,n,ne,n,s,n,se,sw,n,n,n,ne,n,n,n,n,ne,n,s,n,n,se,n,n,s,n,n,n,n,sw,n,n,n,sw,n,s,sw,n,n,n,ne,n,n,n,n,se,n,sw,n,n,n,n,n,n,n,n,n,se,n,n,n,se,n,ne,n,n,n,n,n,sw,n,se,n,n,n,n,n,n,n,nw,n,n,n,n,n,n,n,n,s,n,nw,n,n,n,n,n,n,n,n,n,n,n,n,s,n,n,n,n,nw,se,n,n,n,nw,n,n,se,nw,nw,n,n,n,se,n,n,n,n,n,nw,n,n,n,ne,s,nw,n,se,se,se,ne,ne,n,n,n,n,nw,n,n,nw,n,nw,n,n,n,n,nw,n,se,n,n,n,n,n,n,nw,n,n,n,n,s,n,n,nw,n,n,nw,n,n,n,se,nw,n,n,sw,s,n,n,ne,s,n,n,n,n,n,sw,n,n,n,n,n,nw,s,n,nw,s,n,n,n,nw,nw,nw,n,nw,sw,n,nw,n,n,n,se,nw,nw,n,n,n,nw,n,n,n,n,nw,nw,n,n,sw,nw,n,n,nw,n,n,n,ne,n,n,nw,nw,n,n,n,n,se,s,s,ne,n,n,se,n,n,sw,nw,n,nw,n,nw,n,nw,n,ne,n,n,s,sw,nw,n,n,n,nw,ne,nw,nw,n,n,n,ne,nw,n,n,n,n,n,nw,n,n,nw,nw,sw,nw,n,n,n,n,n,n,n,n,n,s,nw,nw,nw,s,n,nw,n,n,nw,n,nw,n,n,n,n,nw,n,sw,n,s,s,nw,ne,n,n,nw,n,nw,sw,n,n,ne,n,n,ne,n,n,n,sw,nw,n,nw,s,nw,s,s,nw,n,nw,n,n,n,se,s,s,nw,n,n,n,nw,n,n,nw,nw,sw,nw,n,n,n,nw,nw,se,n,nw,n,nw,s,nw,n,n,nw,ne,s,nw,n,s,nw,n,nw,ne,n,nw,nw,n,n,se,s,n,n,sw,nw,n,nw,n,nw,nw,ne,nw,n,n,nw,nw,nw,nw,nw,n,nw,nw,n,n,nw,nw,s,nw,n,se,n,se,n,n,nw,nw,n,nw,n,n,nw,nw,se,n,n,n,se,se,nw,n,nw,s,n,n,n,ne,nw,ne,nw,s,nw,nw,n,nw,nw,n,se,n,nw,nw,nw,nw,nw,nw,ne,sw,s,n,ne,nw,nw,n,nw,n,n,n,n,n,n,n,se,nw,nw,nw,nw,n,n,n,se,nw,sw,nw,se,n,sw,n,nw,nw,nw,se,n,s,nw,ne,nw,nw,s,n,ne,nw,s,nw,nw,n,n,nw,nw,nw,n,nw,nw,nw,nw,n,sw,sw,nw,n,nw,n,n,nw,ne,se,n,nw,nw,n,n,nw,nw,s,n,n,n,n,n,n,sw,nw,s,nw,s,nw,n,ne,nw,nw,n,nw,nw,nw,n,n,nw,nw,sw,nw,nw,nw,nw,nw,nw,nw,nw,n,nw,nw,nw,s,nw,nw,n,nw,nw,n,nw,nw,sw,n,n,nw,nw,se,nw,n,nw,n,s,nw,n,n,s,nw,nw,n,nw,nw,nw,nw,nw,nw,n,nw,nw,nw,nw,n,n,nw,nw,nw,sw,nw,nw,nw,nw,ne,nw,n,nw,ne,se,nw,n,n,nw,nw,n,n,n,nw,ne,n,nw,nw,nw,se,nw,nw,nw,nw,nw,se,nw,nw,s,n,nw,n,nw,n,nw,nw,sw,n,n,nw,nw,ne,nw,n,n,nw,n,nw,nw,nw,se,ne,n,n,nw,n,s,nw,n,nw,ne,ne,nw,nw,nw,nw,n,n,nw,nw,nw,nw,n,nw,nw,nw,nw,nw,nw,nw,nw,nw,ne,nw,n,n,nw,nw,n,nw,se,sw,n,n,nw,s,nw,nw,n,nw,nw,nw,nw,nw,n,nw,se,n,nw,nw,s,nw,nw,nw,n,nw,nw,nw,nw,nw,nw,n,ne,nw,nw,nw,nw,nw,nw,se,n,nw,nw,s,s,n,nw,sw,n,nw,nw,n,nw,se,nw,nw,nw,n,nw,s,nw,nw,nw,nw,se,nw,n,nw,s,nw,nw,nw,nw,ne,n,se,ne,nw,nw,n,nw,nw,nw,nw,nw,nw,nw,sw,nw,ne,nw,ne,nw,nw,sw,nw,nw,nw,s,n,nw,nw,nw,nw,nw,nw,nw,sw,nw,nw,nw,nw,nw,nw,s,nw,nw,nw,nw,nw,nw,n,nw,ne,nw,nw,nw,nw,nw,sw,nw,s,nw,nw,s,nw,sw,s,nw,nw,nw,nw,nw,nw,nw,s,s,nw,nw,nw,se,nw,nw,ne,nw,nw,se,nw,nw,nw,nw,nw,ne,nw,s,nw,sw,s,sw,n,nw,sw,nw,nw,nw,sw,nw,nw,n,nw,nw,nw,nw,nw,nw,nw,nw,se,sw,nw,nw,n,n,nw,sw,nw,nw,n,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,sw,nw,nw,nw,nw,nw,nw,se,nw,nw,n,sw,nw,se,n,sw,nw,nw,nw,nw,s,nw,nw,nw,nw,nw,nw,nw,nw,nw,se,ne,sw,ne,s,nw,nw,nw,nw,ne,nw,nw,nw,nw,nw,nw,se,nw,sw,nw,se,nw,nw,nw,nw,sw,nw,nw,nw,nw,nw,nw,nw,nw,sw,sw,nw,nw,nw,nw,sw,nw,nw,nw,s,sw,sw,sw,sw,nw,sw,sw,nw,se,nw,nw,nw,n,nw,nw,nw,n,nw,sw,nw,nw,nw,nw,s,nw,nw,nw,ne,nw,nw,ne,nw,nw,sw,n,nw,nw,nw,sw,nw,nw,nw,nw,nw,sw,sw,nw,s,sw,ne,nw,nw,n,nw,nw,nw,nw,sw,nw,sw,nw,n,nw,nw,nw,nw,nw,s,nw,sw,nw,nw,s,nw,sw,sw,nw,sw,nw,nw,sw,sw,nw,nw,sw,ne,n,nw,ne,sw,s,nw,sw,nw,se,n,nw,sw,nw,nw,nw,nw,nw,nw,nw,sw,sw,sw,sw,sw,sw,nw,nw,s,sw,nw,nw,sw,nw,nw,s,s,sw,nw,nw,sw,s,nw,n,nw,nw,nw,nw,nw,nw,ne,sw,ne,nw,nw,nw,nw,sw,nw,se,nw,sw,nw,nw,nw,nw,nw,sw,nw,nw,nw,sw,nw,s,sw,nw,nw,nw,nw,nw,sw,ne,n,nw,nw,se,nw,sw,nw,ne,nw,nw,nw,sw,sw,n,nw,nw,n,sw,ne,nw,se,sw,se,sw,sw,nw,nw,sw,nw,se,sw,s,n,nw,sw,sw,n,sw,n,sw,n,sw,nw,sw,sw,nw,sw,nw,ne,nw,n,nw,nw,se,nw,nw,sw,nw,sw,s,nw,sw,nw,sw,nw,nw,sw,nw,sw,nw,n,nw,n,se,nw,ne,nw,nw,sw,sw,nw,sw,nw,nw,n,nw,nw,sw,nw,n,sw,sw,se,sw,nw,sw,nw,nw,nw,sw,sw,sw,nw,nw,sw,nw,sw,nw,s,se,n,sw,ne,nw,sw,sw,sw,sw,nw,nw,nw,nw,sw,se,sw,nw,ne,sw,nw,sw,nw,nw,nw,se,nw,sw,nw,sw,nw,nw,sw,sw,nw,n,sw,nw,nw,sw,nw,nw,sw,nw,sw,sw,sw,sw,nw,sw,sw,sw,sw,se,nw,nw,sw,nw,sw,sw,se,nw,nw,nw,nw,nw,nw,s,sw,sw,sw,nw,nw,n,nw,nw,nw,sw,nw,sw,sw,ne,ne,s,nw,se,nw,sw,nw,nw,nw,nw,sw,nw,nw,nw,ne,ne,sw,nw,nw,nw,nw,nw,ne,sw,nw,nw,sw,nw,nw,se,sw,nw,nw,nw,sw,n,sw,sw,sw,ne,nw,nw,sw,nw,nw,nw,sw,nw,sw,nw,n,nw,n,nw,ne,ne,nw,nw,sw,nw,sw,sw,sw,nw,sw,se,nw,nw,nw,sw,sw,sw,sw,nw,sw,nw,sw,nw,sw,sw,sw,sw,sw,se,nw,nw,nw,sw,sw,sw,nw,nw,nw,nw,sw,sw,sw,n,nw,nw,sw,sw,ne,nw,sw,nw,sw,nw,sw,n,nw,sw,nw,sw,s,s,sw,nw,nw,sw,nw,nw,sw,ne,sw,se,sw,nw,nw,sw,nw,nw,n,nw,nw,nw,sw,nw,sw,nw,se,sw,se,sw,sw,sw,se,sw,sw,s,nw,sw,nw,nw,ne,nw,sw,sw,sw,sw,nw,nw,sw,n,sw,sw,sw,se,nw,ne,ne,sw,nw,se,nw,sw,s,nw,s,sw,nw,n,s,sw,s,sw,sw,sw,nw,nw,sw,nw,s,se,sw,n,sw,sw,sw,sw,sw,sw,ne,nw,sw,se,sw,s,nw,sw,sw,nw,ne,nw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,nw,se,nw,sw,nw,sw,sw,sw,sw,sw,sw,sw,nw,nw,sw,nw,sw,sw,sw,sw,ne,sw,s,sw,nw,ne,sw,nw,nw,sw,sw,sw,sw,sw,nw,sw,nw,sw,s,nw,sw,sw,nw,nw,nw,sw,sw,ne,sw,sw,sw,sw,nw,sw,sw,se,sw,sw,sw,n,nw,nw,sw,n,nw,sw,ne,nw,sw,sw,sw,sw,nw,nw,nw,nw,sw,nw,nw,s,sw,nw,nw,sw,sw,ne,sw,sw,sw,sw,s,se,sw,sw,sw,n,nw,ne,nw,nw,sw,sw,s,se,s,sw,nw,nw,nw,sw,sw,se,nw,nw,sw,sw,nw,sw,sw,nw,sw,nw,sw,se,sw,sw,nw,s,nw,nw,n,sw,ne,nw,s,sw,sw,sw,sw,s,sw,nw,se,n,sw,nw,sw,sw,sw,sw,sw,sw,s,nw,nw,ne,n,sw,sw,sw,sw,sw,nw,nw,sw,nw,nw,nw,sw,nw,n,sw,se,sw,sw,sw,sw,sw,sw,nw,sw,s,nw,sw,sw,sw,sw,se,sw,n,ne,sw,sw,sw,sw,se,sw,sw,nw,n,ne,sw,se,ne,sw,sw,sw,nw,ne,s,sw,sw,nw,sw,sw,nw,nw,sw,sw,s,sw,sw,sw,sw,sw,sw,se,nw,sw,sw,sw,sw,se,sw,sw,sw,sw,sw,n,sw,sw,sw,nw,se,sw,sw,se,sw,sw,nw,sw,sw,sw,sw,sw,ne,nw,sw,n,sw,sw,nw,sw,sw,sw,sw,ne,sw,se,s,sw,n,sw,sw,sw,sw,sw,sw,s,sw,sw,sw,sw,nw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,se,nw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,ne,n,nw,ne,sw,sw,sw,sw,sw,sw,ne,sw,sw,sw,ne,sw,sw,ne,sw,sw,nw,sw,sw,nw,sw,sw,se,sw,sw,sw,nw,sw,sw,n,sw,sw,sw,sw,sw,ne,ne,sw,sw,s,sw,sw,sw,sw,sw,sw,sw,se,sw,sw,se,sw,ne,sw,sw,sw,se,s,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,nw,nw,sw,sw,ne,sw,sw,sw,sw,se,sw,sw,n,sw,sw,sw,sw,s,sw,sw,sw,s,sw,sw,sw,sw,se,sw,sw,sw,s,sw,sw,ne,sw,sw,sw,ne,se,sw,sw,sw,sw,sw,sw,nw,s,sw,s,sw,sw,sw,sw,sw,sw,n,sw,sw,n,sw,sw,sw,se,sw,sw,sw,sw,sw,sw,s,s,sw,ne,sw,n,sw,s,sw,nw,sw,sw,sw,ne,sw,sw,nw,sw,sw,ne,sw,sw,sw,sw,sw,ne,sw,sw,sw,sw,sw,sw,se,sw,sw,sw,sw,sw,sw,sw,sw,sw,s,sw,sw,sw,sw,sw,sw,se,sw,s,sw,sw,sw,sw,sw,sw,sw,se,sw,s,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,nw,sw,nw,sw,sw,sw,se,sw,sw,sw,sw,sw,sw,sw,sw,sw,sw,s,sw,nw,sw,sw,se,sw,sw,sw,nw,sw,nw,sw,sw,sw,sw,sw,sw,s,sw,s,sw,sw,sw,sw,sw,sw,s,sw,sw,nw,sw,sw,sw,sw,sw,s,sw,s,n,sw,sw,sw,nw,sw,ne,sw,sw,sw,sw,sw,s,sw,nw,sw,n,s,sw,s,sw,n,se,sw,sw,sw,sw,sw,sw,sw,n,sw,n,sw,n,s,sw,nw,sw,sw,s,sw,sw,n,sw,sw,sw,nw,sw,sw,s,sw,sw,sw,sw,sw,sw,sw,sw,s,s,se,se,sw,sw,sw,sw,sw,sw,sw,se,s,sw,sw,sw,s,sw,n,sw,sw,sw,sw,sw,sw,sw,sw,sw,s,sw,nw,sw,sw,sw,sw,s,ne,ne,sw,ne,sw,se,sw,nw,sw,sw,sw,sw,sw,s,nw,s,sw,s,sw,sw,sw,sw,n,n,sw,se,n,sw,sw,sw,sw,sw,sw,ne,s,ne,s,sw,sw,sw,sw,s,sw,sw,nw,s,sw,sw,se,sw,sw,s,nw,sw,sw,sw,s,ne,sw,sw,sw,n,n,sw,sw,sw,nw,sw,s,ne,sw,sw,sw,sw,s,s,s,nw,s,s,sw,ne,s,sw,sw,sw,s,nw,sw,sw,sw,s,sw,se,s,s,s,s,sw,s,sw,sw,sw,sw,sw,sw,s,sw,s,sw,s,sw,ne,sw,sw,se,sw,sw,s,sw,s,sw,s,se,s,sw,sw,s,s,sw,sw,s,sw,sw,sw,sw,s,s,sw,ne,sw,n,s,n,se,sw,s,sw,s,sw,n,ne,sw,se,sw,ne,sw,s,s,s,sw,nw,sw,sw,s,sw,sw,sw,s,nw,sw,sw,n,sw,sw,sw,n,ne,s,sw,sw,nw,sw,nw,sw,s,n,s,ne,sw,s,s,s,n,nw,sw,sw,sw,n,nw,s,sw,sw,sw,se,s,sw,sw,sw,se,sw,ne,ne,sw,sw,s,sw,sw,s,n,sw,nw,sw,sw,sw,s,nw,s,s,sw,sw,s,sw,sw,s,n,sw,s,sw,s,s,s,s,s,s,s,s,se,ne,s,ne,sw,sw,s,s,sw,sw,s,nw,s,sw,sw,sw,sw,s,n,s,nw,sw,sw,sw,sw,sw,s,sw,s,s,s,ne,se,s,sw,sw,sw,sw,sw,sw,s,s,se,sw,sw,s,s,sw,sw,sw,se,sw,s,sw,se,sw,s,nw,sw,se,sw,sw,ne,sw,s,s,sw,s,s,nw,sw,sw,s,se,sw,s,s,sw,ne,s,sw,s,s,s,sw,s,sw,s,sw,s,sw,sw,ne,s,s,s,s,sw,nw,ne,s,sw,sw,s,sw,sw,s,nw,s,s,se,ne,n,sw,sw,sw,sw,s,sw,s,sw,se,s,sw,se,sw,s,sw,sw,sw,sw,sw,sw,s,se,sw,sw,se,nw,sw,sw,sw,s,ne,sw,s,sw,sw,s,se,sw,nw,n,sw,sw,nw,sw,sw,sw,sw,sw,ne,n,s,s,sw,sw,sw,n,s,s,s,ne,sw,ne,nw,sw,s,s,se,ne,n,ne,sw,sw,sw,sw,se,sw,sw,sw,sw,sw,sw,sw,s,s,sw,s,s,nw,s,s,ne,sw,s,sw,sw,s,s,s,s,s,s,sw,s,sw,s,sw,s,s,s,s,sw,ne,sw,s,s,n,s,s,n,sw,s,s,sw,s,sw,s,sw,sw,sw,sw,n,s,sw,sw,s,sw,s,ne,ne,sw,s,nw,s,s,se,s,sw,sw,sw,sw,s,s,sw,sw,sw,s,n,sw,s,s,sw,s,s,sw,sw,sw,n,sw,sw,sw,s,s,sw,sw,s,sw,s,sw,sw,nw,s,sw,sw,sw,s,sw,sw,s,sw,sw,s,sw,s,s,s,s,sw,sw,sw,s,s,sw,sw,n,s,nw,sw,sw,n,sw,s,s,s,s,n,sw,sw,s,sw,sw,s,s,s,s,s,s,s,sw,sw,s,se,s,s,sw,s,s,sw,nw,s,s,sw,s,s,s,s,sw,s,sw,sw,sw,s,ne,sw,sw,s,s,s,s,nw,s,s,sw,s,sw,s,s,sw,s,s,s,s,s,s,sw,sw,s,se,nw,s,sw,sw,s,sw,s,s,sw,sw,s,s,s,s,s,sw,sw,s,s,s,s,sw,s,sw,sw,sw,n,ne,s,s,s,sw,sw,sw,s,s,s,s,s,ne,se,ne,s,ne,s,sw,s,sw,sw,sw,s,nw,sw,n,sw,sw,ne,se,sw,s,sw,s,se,s,n,n,s,s,s,ne,s,sw,s,s,s,sw,sw,s,s,s,s,s,s,sw,sw,sw,sw,s,sw,s,sw,sw,s,s,sw,se,s,sw,s,s,nw,s,s,s,sw,s,sw,sw,s,nw,s,sw,nw,s,sw,s,se,s,s,s,s,s,nw,s,s,n,s,nw,s,s,sw,nw,s,s,sw,s,s,s,s,sw,s,s,s,n,sw,s,s,ne,se,s,nw,s,sw,sw,n,s,ne,s,s,ne,sw,s,s,sw,s,sw,s,sw,nw,s,n,sw,s,n,sw,s,sw,s,s,nw,s,sw,n,sw,s,n,sw,ne,s,sw,sw,nw,s,sw,s,nw,s,sw,s,sw,s,s,sw,ne,s,s,s,sw,s,sw,ne,sw,ne,sw,n,s,s,s,s,s,sw,s,nw,s,nw,s,nw,se,s,sw,sw,n,s,s,s,sw,ne,s,sw,s,ne,sw,s,s,s,ne,s,s,s,s,nw,s,s,se,s,s,s,sw,s,s,s,s,s,s,n,s,ne,s,sw,s,se,se,sw,ne,s,s,n,sw,s,s,s,sw,n,n,n,s,s,s,sw,ne,sw,s,s,s,nw,s,s,s,s,s,sw,sw,s,n,s,s,se,s,s,s,s,s,s,s,sw,s,s,s,sw,s,s,s,s,s,s,nw,n,s,s,s,s,s,s,nw,s,sw,se,s,s,s,sw,s,sw,s,sw,s,se,se,s,s,nw,n,s,s,s,s,s,se,s,sw,se,s,se,s,nw,s,s,s,ne,nw,sw,s,sw,sw,nw,sw,s,n,sw,s,sw,s,s,s,s,se,s,s,nw,n,s,s,s,nw,se,sw,n,se,s,s,s,s,s,n,s,s,s,s,s,s,s,s,s,s,se,s,sw,nw,s,s,s,sw,ne,s,sw,s,s,s,s,s,se,s,s,s,ne,s,s,ne,s,s,s,s,s,s,s,ne,s,s,s,s,s,se,se,ne,sw,s,n,n,n,s,s,s,s,s,n,s,se,s,s,ne,n,s,s,s,ne,s,s,s,ne,se,nw,s,s,nw,s,s,s,s,s,s,s,s,s,se,s,s,sw,s,ne,s,s,sw,s,s,s,sw,s,s,s,s,s,s,s,s,s,s,s,s,s,sw,ne,s,s,s,nw,s,s,s,s,s,s,s,s,s,nw,s,n,s,s,s,s,s,s,s,s,nw,s,s,s,s,s,s,s,s,nw,s,s,s,s,s,se,s,ne,s,se,s,s,s,s,s,ne,s,s,s,s,s,nw,s,s,sw,se,se,s,s,s,s,s,s,s,s,se,nw,s,s,nw,s,s,s,s,s,s,s,s,s,s,s,s,s,se,s,s,ne,s,sw,s,n,s,s,s,s,s,sw,s,nw,s,s,nw,n,s,s,s,se,s,s,ne,n,s,s,s,ne,s,s,s,s,s,sw,ne,se,n,s,n,n,ne,ne,ne,s,se,sw,se,se,sw,ne,se,se,se,se,se,n,se,s,s,se,se,sw,n,s,s,s,se,se,s,s,s,s,s,sw,s,s,s,s,s,sw,s,s,sw,n,se,sw,n,sw,sw,sw,s,sw,sw,sw,se,nw,se,sw,sw,sw,nw,s,nw,sw,n,sw,nw,ne,s,se,nw,nw,sw,s,nw,sw,ne,se,nw,sw,nw,nw,nw,sw,nw,sw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,n,n,nw,n,n,nw,nw,ne,n,n,sw,nw,se,ne,n,nw,n,nw,n,nw,n,n,nw,s,ne,nw,sw,n,nw,nw,n,se,n,nw,n,nw,n,s,n,n,n,n,n,ne,n,n,n,ne,n,n,n,n,n,n,n,n,ne,n,ne,n,n,n,ne,nw,n,ne,n,ne,ne,n,n,n,s,n,n,ne,ne,n,n,ne,n,sw,n,ne,ne,s,ne,ne,sw,se,n,sw,ne,nw,sw,ne,ne,ne,sw,nw,s,ne,s,ne,s,ne,ne,ne,ne,ne,n,ne,ne,ne,ne,ne,ne,ne,ne,se,ne,ne,ne,ne,nw,se,s,ne,se,ne,nw,ne,ne,ne,n,ne,ne,ne,se,se,ne,ne,se,se,ne,sw,se,ne,se,ne,ne,se,se,se,se,se,nw,ne,ne,ne,s,se,ne,ne,s,sw,sw,ne,ne,ne,ne,se,nw,se,ne,n,ne,se,sw,ne,s,se,se,se,se,se,se,se,se,se,se,se,se,se,se,se,se,se,s,se,se,ne,se,ne,n,nw,n,se,se,n,s,se,n,se,se,se,se,se,se,se,se,se,se,se,sw,n,se,se,se,se,se,se,se,se,ne,ne,se,se,se,se,nw,se,ne,se,n,nw,s,n,se,se,se,se,se,se,se,ne,n,se,se,se,sw,sw,sw,se,s,se,s,se,se,se,s,se,se,s,se,se,s,ne,s,se,se,s,se,s,s,n,s,se,sw,se,s,s,sw,se,se,s,sw,ne,se,s,n,se,nw,se,nw,sw,s,n,s,s,s,s,s,s,se,sw,s,se,s,s,s,s,sw,s,s,s,nw,s,s,s,s,s,se,s,se,s,s,s,nw,s,s,se,nw,s,nw,s,nw,nw,n,se,s,s,s,s,s,s,s,s,sw,s,s,s,n,s,s,n,sw,sw,s,s,s,s,n,se,s,n,nw,ne,s,s,s,ne,s,s,s,s,ne,s,s,s,se,s,s,sw,ne,n,ne,nw,sw,s,s,sw,s,s,nw,s,s,s,s,s,s,s,s,s,sw,sw,nw,sw,s,s,s,n,s,s,sw,s,sw,s,s,s,ne,sw,sw,s,sw,s,nw,ne,s,nw,ne,s,s,s,s,s,s,n,s,sw,s,sw,sw,ne,s,s,s,ne,se,s,s,s,sw,sw,se,sw,se,sw,sw,nw,s,nw,s,sw,s,nw,sw,s,sw,sw,s,s,sw,s,sw,s,s,sw,s,s,s,sw,sw,sw,s,s,se,n,s,s,se,s,s,sw,sw,s,sw,s,sw,s,sw,sw,s,nw,s,s,nw,sw,sw,n,nw,n,nw,sw,sw,s,sw,sw,sw,sw,s,se,sw,s,sw,sw,sw,se,ne,sw,s,s,sw,sw,se,sw,ne,sw,sw,sw,sw,sw,sw,sw,ne,sw,sw,sw,sw,sw,sw,s,sw,sw,n,s,sw,sw,nw,sw,sw,sw,sw,ne,s,sw,sw,sw,nw,sw,sw,sw,sw,sw,sw,ne,nw,sw,nw,s,sw,sw,sw,se,s,sw,sw,sw,sw,nw,sw,sw,sw,sw,sw,sw,nw,sw,s,nw,sw,sw,nw,nw,nw,sw,sw,sw,sw,sw,se,sw,sw,sw,sw,n,nw,nw,sw,se,se,se,sw,sw,sw,ne,nw,nw,sw,nw,n,n,s,n,sw,sw,sw,sw,n,se,sw,ne,sw,nw,sw,sw,sw,nw,sw,sw,sw,nw,sw,sw,sw,nw,nw,n,sw,sw,sw,nw,se,nw,sw,nw,sw,sw,sw,sw,nw,s,nw,sw,s,sw,sw,nw,ne,nw,se,sw,se,nw,sw,nw,nw,sw,sw,sw,sw,sw,nw,nw,sw,s,nw,sw,nw,nw,nw,nw,nw,nw,se,sw,se,sw,nw,nw,se,sw,sw,nw,nw,sw,nw,nw,nw,sw,nw,sw,nw,sw,nw,se,sw,nw,sw,sw,sw,nw,sw,sw,sw,sw,sw,sw,nw,nw,nw,nw,nw,sw,sw,n,nw,nw,s,sw,sw,nw,se,nw,n,nw,nw,nw,nw,nw,nw,n,ne,nw,sw,s,nw,sw,nw,nw,s,nw,sw,sw,nw,nw,sw,sw,n,sw,nw,nw,nw,ne,nw,nw,nw,nw,nw,nw,ne,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,nw,s,se,sw,nw,nw,nw,n,nw,n,nw,nw,nw,sw,nw,ne,sw,nw,nw,nw,sw,nw,sw,nw,sw,n,nw,sw,nw,nw,nw,n,se,nw,ne,nw,sw,nw,nw,nw,nw,nw,nw,nw,nw,nw,se,nw,nw,nw,ne,sw,nw,nw,nw,nw,nw,nw,ne,nw,sw,s,nw,nw,nw,nw,nw,n,ne,sw,nw,nw,nw,se,nw,nw,nw,nw,nw,sw,nw,nw,n,nw,sw,nw,s,ne,nw,ne,nw,nw,nw,nw,n,nw,nw,n,nw,nw,nw,sw,nw,nw,nw,ne,nw,nw,nw,se,sw,nw,n,nw,nw,n,se,n,nw,n,s,nw,nw,sw,nw,sw,nw,ne,n,nw,n,ne,sw,ne,nw,nw,nw,nw,n,n,nw,nw,nw,nw,sw,s,nw,nw,n,nw,nw,nw,n,ne,nw,nw,nw,nw,n,se,n,nw,n,n,n,n,n,nw,ne,nw,s,nw,se,n,nw,nw,nw,nw,sw,nw,n,nw,nw,nw,nw,ne,se,nw,ne,nw,nw,se,se,nw,sw,nw,nw,nw,se,n,n,nw,n,nw,nw,n,n,n,nw,nw,nw,n,n,n,se,nw,sw,n,nw,nw,nw,nw,n,nw,nw,n,nw,nw,n,n,n,ne,sw,nw,n,nw,nw,n,nw,nw,se,nw,n,nw,n,s,nw,nw,nw,n,n,nw,n,n,nw,nw,sw,n,nw,s,n,n,n,nw,nw,n,nw,n,n,nw,nw,sw,n,n,se,nw,nw,n,nw,n,n,nw,s,nw,n,nw,n,se,n,n,n,ne,nw,nw,n,sw,n,n,n,n,n,nw,nw,n,n,s,ne,n,n,n,nw,nw,n,n,n,n,sw,n,s,ne,n,nw,n,n,n,n,sw,n,nw,n,nw,nw,ne,n,nw,n,n,n,n,n,n,nw,n,n,nw,nw,se,ne,n,nw,n,n,nw,n,n,nw,n,n,n,sw,nw,n,s,nw,n,n,n,n,n,n,se,n,s,n,n,n,n,nw,nw,n,n,s,n,n,n,nw,nw,n,nw,n,n,sw,n,n,n,nw,ne,n,n,n,n,nw,n,nw,n,nw,s,n,n,nw,n,n,nw,sw,n,n,ne,ne,n,n,ne,n,n,s,ne,n,n,n,n,n,n,n,n,sw,n,n,n,n,n,n,n,sw,n,n,n,n,n,n,n,n,n,sw,n,n,n,n,n,sw,n,n,n,n,n,ne,n,n,n,n,se,n,n,n,n,n,n,s,se,n,n,n,n,sw,nw,n,n,n,n,n,nw,n,n,n,n,n,n,se,n,n,n,n,n,n,nw,n,n,s,n,n,n,n,nw,n,n,n,n,n,n,sw,n,ne,n,n,sw,n,n,n,n,se,n,n,ne,n,n,n,n,n,s,se,n,n,n,n,se,ne,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,se,n,ne,n,n,n,n,n,n,ne,n,ne,n,n,ne,ne,ne,nw,s,s,n,sw,n,se,n,se,n,ne,n,n,n,n,ne,n,n,n,n,n,ne,n,ne,n,ne,n,sw,ne,ne,s,n,n,n,n,n,n,n,n,n,s,n,ne,n,n,s,sw,ne,nw,n,n,n,ne,n,ne,ne,s,se,n,n,n,n,ne,n,se,n,n,n,n,n,ne,ne,n,ne,ne,n,ne,n,n,ne,n,n,ne,n,nw,n,ne,n,s,n,n,ne,se,sw,n,n,se,ne,n,ne,ne,ne,n,n,n,n,n,n,n,n,sw,nw,n,n,n,n,n,ne,ne,s,se,n,sw,ne,n,se,ne,nw,ne,ne,ne,n,n,n,ne,ne,sw,n,n,n,sw,n,n,ne,n,n,n,se,s,ne,s,ne,ne,ne,se,n,n,sw,n,n,ne,n,s,ne,nw,nw,ne,n,n,n,s,s,n,ne,sw,ne,ne,ne,ne,s,n,ne,ne,ne,ne,sw,ne,n,n,n,n,n,s,ne,ne,ne,ne,se,s,n,n,nw,n,n,n,n,s,s,n,ne,n,n,s,n,ne,ne,n,n,n,ne,n,ne,ne,ne,ne,ne,ne,ne,ne,sw,n,nw,s,ne,ne,n,n,n,ne,n,n,n,ne,sw,nw,se,n,ne,ne,ne,se,n,ne,ne,sw,n,ne,n,n,n,n,ne,sw,ne,ne,n,ne,ne,n,se,ne,ne,se,ne,ne,nw,sw,ne,ne,n,n,ne,sw,ne,n,ne,ne,n,ne,ne,n,ne,ne,ne,ne,ne,n,n,ne,ne,n,ne,ne,ne,ne,se,s,se,ne,n,ne,n,ne,ne,sw,ne,n,ne,ne,n,ne,ne,nw,ne,ne,n,nw,ne,se,ne,ne,ne,ne,ne,sw,nw,ne,ne,ne,ne,ne,ne,ne,n,ne,n,ne,sw,ne,sw,ne,n,ne,se,ne,ne,ne,ne,sw,ne,ne,ne,se,ne,ne,ne,ne,ne,sw,ne,ne,ne,ne,s,ne,ne,n,n,ne,ne,se,ne,ne,s,ne,ne,n,ne,ne,n,ne,se,ne,ne,n,sw,ne,ne,ne,n,ne,sw,ne,ne,nw,ne,ne,ne,n,ne,ne,ne,ne,s,se,ne,ne,ne,se,ne,n,ne,ne,ne,ne,s,ne,ne,ne,ne,ne,nw,ne,ne,ne,ne,ne,nw,ne,ne,nw,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,ne,nw,sw,ne,ne,ne,ne,ne,n,ne,ne,ne,ne,ne,se,ne,sw,ne,sw,ne,ne,ne,ne,ne,ne,ne,ne,s,sw,nw,ne,ne,nw,nw,ne,ne,se,ne,ne,ne,ne,s,ne,ne,ne,ne,ne,ne,ne,ne,se,se,ne,ne,ne,sw,ne,ne,se,ne,ne,ne,se,ne,ne,ne,ne,ne,ne,ne,ne,ne,se,ne,ne,ne,se,s,nw,ne,ne,ne,ne,n,se,ne,sw,s,ne,ne,ne,ne,s,n,ne,ne,ne,se,s,nw,ne,ne,se,ne,ne,ne,ne,nw,ne,ne,ne,ne,ne,n,se,ne,s,ne,ne,ne,ne,se,ne,n,n,ne,ne,ne,ne,ne,ne,sw,ne,ne,ne,ne,ne,ne,s,ne,ne,ne,sw,s,sw,sw,ne,s,ne,ne,se,s,se,ne,ne,ne,ne,ne,nw,ne,ne,ne,ne,ne,ne,se,ne,ne,ne,ne,se,ne,n,se,se,ne,s,ne,ne,ne,s,ne,ne,se,ne,ne,ne,se,ne,ne,ne,ne,se,ne,ne,se,n,se,se,ne,ne,nw,ne,s,ne,se,ne,ne,ne,se,s,se,ne,se,se,ne,se,ne,ne,se,se,ne,ne,se,sw,ne,nw,ne,se,se,ne,se,ne,se,ne,nw,ne,s,ne,sw,ne,se,ne,ne,ne,ne,ne,sw,sw,ne,se,sw,ne,se,ne,se,ne,se,ne,ne,se,sw,se,ne,ne,ne,se,s,ne,ne,ne,ne,ne,ne,ne,n,se,ne,ne,se,ne,sw,se,se,ne,sw,n,se,se,se,se,sw,ne,ne,se,ne,ne,ne,ne,nw,ne,ne,ne,ne,se,se,se,ne,se,se,ne,ne,ne,se,ne,ne,se,ne,ne,ne,nw,ne,ne,ne,ne,se,se,ne,ne,se,ne,ne,se,ne,s,n,se,ne,ne,se,ne,ne,ne,ne,se,se,se,ne,nw,ne,ne,ne,se,se,se,ne,se,ne,se,ne,ne,n,ne,ne,ne,ne,ne,se,se,se,sw,se,ne,se,ne,n,ne,ne,ne,se,ne,se,sw,n,ne,n,ne,se,se,nw,ne,n,nw,ne,ne,ne,ne,ne,ne,ne,ne,se,se,se,ne,ne,s,se,nw,se,ne,ne,s,se,se,ne,sw,ne,se,se,s,se,ne,se,sw,n,ne,ne,se,nw,ne,ne,ne,n,ne,ne,se,ne,ne,ne,se,se,se,ne,se,ne,nw,ne,s,ne,se,ne,ne,se,se,ne,se,ne,se,sw,ne,sw,ne,se,ne,ne,ne,se,se,ne,ne,ne,se,ne,n,se,s,se,ne,ne,n,se,se,se,ne,se,se,se,se,ne,ne,se,se,se,se,ne,se,se,se,ne,n,se,se,se,nw,s,ne,nw,ne,ne,ne,se,se,se,se,se,ne,se,se,n,se,ne,n,ne,sw,ne,ne,se,se,se,se,n,se,n,ne,ne,se,se,se,ne,se,se,ne,se,se,se,nw,se,s,s,se,nw,s,ne,se,nw,ne,ne,se,se,s,se,se,se,ne,ne,se,ne,ne,ne,se,se,se,nw,se,ne,se,ne,nw,se,ne,nw,ne,se,ne,se,se,se,ne,ne,se,se,ne,se,se,se,se,se,se,ne,ne,ne,ne,se,se,se,se,nw,sw,se,ne,se,se,se,se,s,ne,se,nw,ne,se,nw,ne,se,se,se,se,ne,nw,n,se,ne,se,se,nw,n,sw,se,ne,ne,se,nw,n,se,se,se,s,s,ne,se,se,se,se,se,n,ne,se,se,se,se,se,sw,ne,se,se,se,se,se,s,ne,se,se,se,nw,se,se,n,se,se,ne,se,se,se,ne,se,se,nw,se,se,nw,ne,se,se,se,ne,se,se,n,se,se,n,se,se,s,se,se,se,se,ne,se,se,se,se,sw,sw,se,nw,se,se,se,se,se,se,se,se,se,nw,n,se,se,se,s,se,se,se,se,se,se,se,se,s,se,ne,se,se,se,se,se,nw,se,s,se,se,n,se,se,se,s,se,se,s,ne,se,se,se,ne,se,se,nw,se,se,se,nw,se,se,nw,n,nw,se,se,se,se,ne,se,se,se,se,se,se,se,n,se,se,se,se,se,se,nw,se,se,se,se,se,nw,se,nw"
