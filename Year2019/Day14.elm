module Year2019.Day14 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict
import Dict.Extra
import Graph exposing (Graph)
import List.Extra



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    ( RecipeGraph, ProcessingOrder )


type alias Input2 =
    ( RecipeGraph, ProcessingOrder )


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


type alias ProcessingOrder =
    List String


type alias Item =
    ( Int, String )


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
            ( Advent.unsafeToInt quantityString
            , typeString
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
        |> mapVertices Tuple.second
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


compute1 : Input1 -> Output1
compute1 ( graph, order ) =
    let
        _ =
            Debug.log "=======================================" ()
    in
    -- how much "ORE" needed for 1 "FUEL"
    process1 graph order [ ( 1, "FUEL" ) ]


process1 : RecipeGraph -> ProcessingOrder -> List Item -> Int
process1 graph order dependencies =
    let
        allProcessed =
            List.foldl
                (\typeToProcess deps -> process1Type graph order typeToProcess deps)
                dependencies
                order
    in
    case allProcessed of
        [ ( n, "ORE" ) ] ->
            n

        _ ->
            Debug.todo "process1 wat"


process1Type : RecipeGraph -> ProcessingOrder -> String -> List Item -> List Item
process1Type graph order typeToProcess dependencies =
    dependencies
        |> List.concatMap (process1One graph order typeToProcess)
        |> addSame


process1One : RecipeGraph -> ProcessingOrder -> String -> Item -> List Item
process1One graph order typeToProcess (( _, type_ ) as item) =
    if type_ == typeToProcess then
        getDeps graph order item

    else
        [ item ]


getDeps : RecipeGraph -> ProcessingOrder -> Item -> List Item
getDeps graph order item =
    let
        easyDeps =
            Graph.outgoingEdges item graph
    in
    if List.isEmpty easyDeps then
        getTrickyDeps graph order item

    else
        easyDeps


getTrickyDeps : RecipeGraph -> ProcessingOrder -> Item -> List Item
getTrickyDeps graph order (( quantity, type_ ) as item) =
    if type_ == "ORE" then
        [ ( quantity, type_ ) ]

    else
        let
            _ =
                Debug.log "------------------------------" type_
        in
        let
            _ =
                Debug.log "want to produce" quantity
        in
        let
            allPossibilities : List Item
            allPossibilities =
                graph
                    |> Graph.edges
                    |> List.filterMap
                        (\{ from, to } ->
                            if Tuple.second from == type_ then
                                Just from

                            else
                                Nothing
                        )
                    |> List.Extra.unique

            possibility : Item
            possibility =
                -- TODO use all of them, not just the first one?
                allPossibilities
                    |> List.head
                    |> Advent.unsafeMaybe "possibility head"

            ( possibleQuantity, _ ) =
                possibility

            _ =
                Debug.log "can produce" possibleQuantity
        in
        let
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

            _ =
                if overflow == 0 then
                    0

                else
                    Debug.log "will waste" overflow
        in
        deps
            |> List.map (\( depQuantity, depType_ ) -> ( depQuantity * amountNeeded, depType_ ))


addSame : List Item -> List Item
addSame dependencies =
    dependencies
        |> Dict.Extra.groupBy Tuple.second
        |> Dict.map
            (\type_ sameDeps ->
                sameDeps
                    |> List.map Tuple.first
                    |> List.sum
                    |> (\quantity -> ( quantity, type_ ))
            )
        |> Dict.values


compute2 : Input2 -> Output2
compute2 input =
    -1



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [ --    [ Test "example 1"
      --        """10 ORE => 10 A
      --1 ORE => 1 B
      --7 A, 1 B => 1 C
      --7 A, 1 C => 1 D
      --7 A, 1 D => 1 E
      --7 A, 1 E => 1 FUEL"""
      --        Nothing
      --        -- Just "parsed-input"
      --        31
      --    , Test "example 2"
      --        """9 ORE => 2 A
      --8 ORE => 3 B
      --7 ORE => 5 C
      --3 A, 4 B => 1 AB
      --5 B, 7 C => 1 BC
      --4 C, 1 A => 1 CA
      --2 AB, 3 BC, 4 CA => 1 FUEL"""
      --        Nothing
      --        165
      --    ,
      Test "example 3"
        """157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"""
        Nothing
        13312
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



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
