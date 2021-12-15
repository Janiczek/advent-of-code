module Year2019.Day14 exposing (..)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)
import Dict.Extra
import Graph exposing (Graph)
import List.Extra
import Maybe.Extra
import Set



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    ( RecipeGraph, ProcessingOrder )


type alias Input2 =
    ( RecipeGraph, ProcessingOrder )


type alias Conversion =
    { before : Dict String Int, after : Dict String Int }


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


type alias ProcessingOrder =
    List String


type alias Item =
    ( String, Int )


type alias RawRecipe =
    { inputs : List Item, output : Item }


type alias RecipeGraph =
    -- The graph relation is "needs", not "provides"
    Graph Item ()


parseRawRecipes : String -> List RawRecipe
parseRawRecipes string =
    string
        |> String.lines
        |> List.map parseRawRecipe


parseRawRecipe : String -> RawRecipe
parseRawRecipe string =
    case String.split " => " string of
        [ inputsString, outputString ] ->
            { inputs =
                inputsString
                    |> String.split ", "
                    |> List.map parseItem
            , output = parseItem outputString
            }

        _ ->
            Debug.todo "parseRawRecipe wat"


parseItem : String -> Item
parseItem string =
    case String.split " " string of
        [ quantityString, typeString ] ->
            ( typeString
            , Advent.unsafeToInt quantityString
            )

        _ ->
            Debug.todo "parseItem wat"


rawRecipesToGraph : List RawRecipe -> RecipeGraph
rawRecipesToGraph recipes =
    -- TODO Graph.union
    List.foldl
        addRecipeToGraph
        Graph.empty
        recipes


addRecipeToGraph : RawRecipe -> RecipeGraph -> RecipeGraph
addRecipeToGraph { inputs, output } graph =
    List.foldl
        (\input -> Graph.addEdge output input ())
        graph
        inputs


graphToProcessingOrder : RecipeGraph -> ProcessingOrder
graphToProcessingOrder graph =
    graph
        |> mapVertices Tuple.first
        |> topologicalSort


mapVertices : (vertex1 -> vertex2) -> Graph vertex1 edge -> Graph vertex2 edge
mapVertices fn graph =
    -- TODO use in the Graph library
    let
        { vertices, edges } =
            Graph.verticesAndEdges graph

        newVertices =
            List.map fn vertices

        newEdges =
            List.map
                (\{ from, to, data } ->
                    { from = fn from
                    , to = fn to
                    , data = data
                    }
                )
                edges
    in
    Graph.fromVerticesAndEdges
        newVertices
        newEdges


topologicalSort : Graph vertex edge -> List vertex
topologicalSort graph =
    -- TODO make this into Graph.topologicalSort
    -- Kahn's algorithm
    let
        { vertices, edges } =
            Graph.verticesAndEdges graph

        verticesWithNoIncomingEdges =
            vertices
                |> List.filter (\vertex -> List.isEmpty (List.filter (\{ to } -> to == vertex) edges))
    in
    topologicalSortHelp [] verticesWithNoIncomingEdges graph


topologicalSortHelp : List vertex -> List vertex -> Graph vertex edge -> List vertex
topologicalSortHelp done verticesWithNoIncomingEdges graph =
    case verticesWithNoIncomingEdges of
        [] ->
            List.reverse done

        vertex :: rest ->
            let
                newDone =
                    vertex :: done

                outgoing =
                    Graph.outgoingEdges vertex graph

                newGraph =
                    Graph.removeVertex vertex graph

                newEdges =
                    Graph.edges newGraph

                verticesToAdd =
                    outgoing
                        |> List.filter (\to -> not (List.any (\edge -> edge.to == to) newEdges))

                newRest =
                    rest ++ verticesToAdd
            in
            topologicalSortHelp newDone newRest newGraph


parse1 : String -> Input1
parse1 string =
    let
        graph =
            string
                |> parseRawRecipes
                |> rawRecipesToGraph

        processingOrder =
            graphToProcessingOrder graph
    in
    ( graph, processingOrder )


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


add : Item -> Dict String Int -> Dict String Int
add ( type_, amount ) inventory =
    if amount == 0 then
        inventory

    else
        Dict.update
            type_
            (Maybe.withDefault 0 >> (+) amount >> Just)
            inventory


addMultiple : List Item -> Dict String Int -> Dict String Int
addMultiple items inventory =
    List.foldl add inventory items


remove : Item -> Dict String Int -> Maybe (Dict String Int)
remove ( type_, amount ) inventory =
    if amount == 0 then
        Just inventory

    else
        Dict.get type_ inventory
            |> Maybe.andThen
                (\amountAvailable ->
                    case compare amountAvailable amount of
                        LT ->
                            Nothing

                        EQ ->
                            Just <| Dict.remove type_ inventory

                        GT ->
                            Just <| Dict.insert type_ (amountAvailable - amount) inventory
                )


removeMultiple : List Item -> Dict String Int -> Maybe (Dict String Int)
removeMultiple items inventory =
    List.foldl
        (\item maybeInv -> Maybe.andThen (remove item) maybeInv)
        (Just inventory)
        items


eliminateType : Input1 -> String -> Conversion -> Conversion
eliminateType input typeToProcess conversion =
    conversion.before
        |> Dict.toList
        |> List.filterMap
            (\(( type_, _ ) as item) ->
                if type_ == typeToProcess then
                    Just <| getDeps input item

                else
                    Nothing
            )
        |> List.foldl
            (\( leftoverAmount, deps_ ) { before, after } ->
                { before =
                    before
                        |> addMultiple deps_
                , after =
                    after
                        |> add ( typeToProcess, leftoverAmount )
                }
            )
            { conversion | before = Dict.remove typeToProcess conversion.before }


getDeps : Input1 -> Item -> ( Int, List Item )
getDeps (( graph, _ ) as input) item =
    let
        easyDeps =
            Graph.outgoingEdges item graph
    in
    if List.isEmpty easyDeps then
        getTrickyDeps input item

    else
        ( 0, easyDeps )


getTrickyDeps : Input1 -> Item -> ( Int, List Item )
getTrickyDeps ( graph, order ) (( type_, quantity ) as item) =
    if type_ == "ORE" then
        ( 0, [ item ] )

    else
        let
            allPossibilities : List Item
            allPossibilities =
                graph
                    |> Graph.edges
                    |> List.filterMap
                        (\{ from, to } ->
                            if Tuple.first from == type_ then
                                Just from

                            else
                                Nothing
                        )
                    |> List.Extra.unique

            possibility : Item
            possibility =
                allPossibilities
                    |> List.head
                    |> Advent.unsafeMaybe "possibility head"

            ( _, possibleQuantity ) =
                possibility

            deps : List Item
            deps =
                graph
                    |> Graph.outgoingEdges possibility

            amountNeeded : Int
            amountNeeded =
                (toFloat quantity / toFloat possibleQuantity)
                    |> ceiling

            overflow : Int
            overflow =
                amountNeeded * possibleQuantity - quantity
        in
        ( overflow
        , deps
            |> List.map (Tuple.mapSecond ((*) amountNeeded))
        )



------------------- BETTER ABSTRACTION --------------------------


{-| Automatically adds "ORE" to the deps and removes "FUEL" from them.

    getToNFuel example13312 1 []
    --> { before = [ ( "ORE", 13312 ) ]
    --  , after =
    --      [ ( "FUEL", 1 )
    --      , ( "K", 3 )
    --      , ( "P", 3 )
    --      , ( "N", 4 )
    --      , ( "D", 5 )
    --      , ( "Q", 8 )
    --      ]
    --  }

-}
getToNFuel : Input1 -> Int -> List String -> Conversion
getToNFuel (( _, order ) as input) fuelAmount allowedDeps =
    let
        deps =
            allowedDeps
                |> Set.fromList
                |> Set.insert "ORE"
                |> Set.remove "FUEL"
    in
    List.foldl
        (eliminateType input)
        { before = Dict.singleton "FUEL" fuelAmount -- only the `allowedDeps` will be here after we finish
        , after = Dict.singleton "FUEL" fuelAmount -- this FUEL and some leftovers will be here
        }
        (List.filter (\type_ -> not (Set.member type_ deps)) order)



--------------- USAGE OF THE BETTER ABSTRACTION -----------------------


compute1 : Input1 -> Output1
compute1 input =
    oreNeededForNFuel input 1


oreNeededForNFuel : Input1 -> Int -> Int
oreNeededForNFuel input n =
    getToNFuel input n []
        |> .before
        |> Dict.get "ORE"
        |> Advent.unsafeMaybe ("ore needed for n fuel " ++ String.fromInt n)


compute2 : Input2 -> Output2
compute2 input =
    let
        go current increment foundMax =
            let
                oreNeeded =
                    oreNeededForNFuel input current
            in
            if oreNeeded == 1000000000000 then
                current

            else
                let
                    tooHigh =
                        oreNeeded > 1000000000000
                in
                if tooHigh && increment == 1 then
                    current - 1

                else
                    let
                        newFoundMax =
                            tooHigh || foundMax

                        newIncrement =
                            if tooHigh then
                                max 1 (increment // 2)

                            else if newFoundMax then
                                increment

                            else
                                increment * 2

                        newCurrent =
                            if tooHigh then
                                current - newIncrement

                            else
                                current + increment
                    in
                    go newCurrent newIncrement newFoundMax
    in
    go 1 1 False



-- 4. TESTS (uh-oh, is this problem a hard one?)


example13312 : Input1
example13312 =
    parse1
        """157 ORE => 5 N
165 ORE => 6 D
44 X, 5 K, 1 Q, 29 N, 9 G, 48 H => 1 FUEL
12 H, 1 G, 8 P => 9 Q
179 ORE => 7 P
177 ORE => 5 H
7 D, 7 P => 2 X
165 ORE => 2 G
3 D, 7 N, 5 H, 10 P => 8 K"""


tests1 : List (Test Input1 Output1)
tests1 =
    [ Test "example1 3"
        """157 ORE => 5 N
165 ORE => 6 D
44 X, 5 K, 1 Q, 29 N, 9 G, 48 H => 1 FUEL
12 H, 1 G, 8 P => 9 Q
179 ORE => 7 P
177 ORE => 5 H
7 D, 7 P => 2 X
165 ORE => 2 G
3 D, 7 N, 5 H, 10 P => 8 K"""
        Nothing
        13312
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    [ Test "example2 1"
        """157 ORE => 5 N
165 ORE => 6 D
44 X, 5 K, 1 Q, 29 N, 9 G, 48 H => 1 FUEL
12 H, 1 G, 8 P => 9 Q
179 ORE => 7 P
177 ORE => 5 H
7 D, 7 P => 2 X
165 ORE => 2 G
3 D, 7 N, 5 H, 10 P => 8 K"""
        Nothing
        82892753
    ]



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
9 RJLWC, 9 RJCH => 9 QWFH
1 XZVHQ, 9 SPQR, 2 WKGVW => 5 KPZB
12 HPRPM, 4 GTZCK => 7 DJNDX
7 JKRV, 3 FKTLR, 19 FDSBZ => 9 HPRPM
9 VTCRJ => 4 SPSW
2 FDSBZ, 1 FKTLR => 6 KBJF
9 SPSW => 9 QHVSJ
5 TFPNF, 11 MNMBX, 1 QCMJ, 13 TXPL, 1 DJNDX, 9 XZVHQ, 2 WKGVW, 2 VQPX => 8 GPKR
10 DWTC, 8 DSPJG => 4 QCMJ
100 ORE => 9 XZDP
3 DBRBD => 4 DKRX
37 JKRV, 5 FKTLR => 7 VXZN
3 HWDS, 2 ZRBN => 8 XZVHQ
15 QNXZV, 53 VXZN, 3 LJQH, 13 FKXVQ, 6 DZGN, 17 MNMBX, 16 GPKR, 8 HWJVK => 1 FUEL
8 GSLWP => 7 PWTFL
4 HVPWG => 9 JKRV
5 NVWGS, 1 QWFH, 9 CWZRS => 2 XPMV
6 ZRBN => 4 JZDB
36 BWXWC, 14 HKFD => 3 FMNK
3 FMNK, 2 SPSW, 16 WKGVW => 6 VQPX
1 DWTC => 9 VMHM
3 HPRPM, 1 DWTC => 5 TXPL
1 KBJF, 2 ZSKSW => 1 MNMBX
5 JZDB => 4 FDSBZ
2 FKXVQ => 9 ZTFZG
17 XZDP => 2 HKFD
7 VMHM => 3 FGQF
1 JKRV => 8 CWZRS
1 WKGVW, 2 SPSW => 6 VLQP
3 ZRBN => 3 ZSKSW
7 VXZN, 7 TGLHX => 5 NVWGS
10 VLQP, 18 FGQF => 4 DBRBD
8 VMHM => 8 SPQR
1 KPZB, 4 GQGB, 3 WKGVW => 1 FDSZX
2 VXZN => 8 VTCRJ
3 RJLWC => 2 GQGB
6 TXPL => 4 DSPJG
2 ZTFZG => 8 TJLW
1 MPSPS => 3 BWXWC
5 FMNK, 4 ZSKSW => 5 RWKWD
137 ORE => 3 MPSPS
1 VTCRJ, 8 QWFH => 2 GKVQK
8 RJLWC => 8 TFPNF
7 TJLW, 1 TFPNF, 16 VQPX, 4 DBRBD, 4 GTZCK, 5 XPMV, 1 FDSZX => 6 DZGN
1 HVPWG => 7 RJLWC
18 HVPWG, 9 BWXWC => 4 GSLWP
107 ORE => 8 RJCH
1 RJCH => 2 ZRBN
2 GSLWP, 18 RWKWD, 1 QWFH => 5 LJQH
3 VXZN, 1 FMNK => 4 TGLHX
3 HKFD, 6 FMNK => 3 FKTLR
3 MPSPS => 4 HVPWG
27 PWTFL, 15 ZTFZG, 6 QHVSJ, 14 DJNDX, 9 RWKWD, 2 MNMBX, 4 DKRX => 6 QNXZV
1 ZSKSW, 9 KBJF => 3 FKXVQ
2 FDSBZ => 4 DWTC
3 HPRPM => 5 HWDS
1 GKVQK, 1 PWTFL => 5 GTZCK
1 FGQF => 5 WKGVW
5 FDSBZ, 7 SPSW => 6 HWJVK
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
