module Grid exposing
    ( Grid
    , addPosition
    , all
    , allNeighbourPositions
    , allNeighbours
    , allNeighboursOrDefault
    , allNeighboursWithPositions
    , allPositions
    , at
    , atOrDefault
    , defaultFn
    , diagonalNeighbourPositions
    , diagonalNeighbours
    , diagonalNeighboursOrDefault
    , diagonalNeighboursWithPositions
    , filter
    , from2DList
    , from2DListWithOrigin
    , fromDict
    , fromList
    , getByDeltas
    , getByDeltasOrDefault
    , init
    , map
    , mapPositions
    , orthogonalNeighbourPositions
    , orthogonalNeighbours
    , orthogonalNeighboursOrDefault
    , orthogonalNeighboursWithPositions
    , set
    , toDict
    , toList
    )

import Dict exposing (Dict)
import Dict.Extra as Dict


type Grid a
    = Grid
        { default : ( Int, Int ) -> a
        , dict : Dict ( Int, Int ) a
        }


init : (( Int, Int ) -> a) -> Grid a
init default =
    Grid
        { default = default
        , dict = Dict.empty
        }


defaultFn : Grid a -> (( Int, Int ) -> a)
defaultFn (Grid g) =
    g.default


fromList : (( Int, Int ) -> a) -> List ( ( Int, Int ), a ) -> Grid a
fromList default list =
    fromDict default (Dict.fromList list)


fromDict : (( Int, Int ) -> a) -> Dict ( Int, Int ) a -> Grid a
fromDict default dict =
    Grid
        { default = default
        , dict = dict
        }


from2DList : (( Int, Int ) -> a) -> List (List a) -> Grid a
from2DList default list2D =
    from2DListWithOrigin default ( 0, 0 ) list2D


from2DListWithOrigin : (( Int, Int ) -> a) -> ( Int, Int ) -> List (List a) -> Grid a
from2DListWithOrigin default ( ox, oy ) list2D =
    list2D
        |> List.indexedMap
            (\y row ->
                row
                    |> List.indexedMap (\x cell -> ( ( ox + x, oy + y ), cell ))
            )
        |> List.concat
        |> fromList default


toList : Grid a -> List ( ( Int, Int ), a )
toList (Grid g) =
    Dict.toList g.dict


toDict : Grid a -> Dict ( Int, Int ) a
toDict (Grid g) =
    g.dict


allPositions : Grid a -> List ( Int, Int )
allPositions (Grid g) =
    Dict.keys g.dict


at : ( Int, Int ) -> Grid a -> Maybe a
at pos (Grid g) =
    Dict.get pos g.dict


atOrDefault : ( Int, Int ) -> Grid a -> a
atOrDefault pos ((Grid g) as g_) =
    at pos g_
        |> Maybe.withDefault (g.default pos)


set : ( Int, Int ) -> a -> Grid a -> Grid a
set pos val (Grid g) =
    Grid { g | dict = Dict.insert pos val g.dict }


map : (( Int, Int ) -> a -> b) -> Grid a -> Grid b
map fn (Grid g) =
    Grid
        { default = \pos -> fn pos (g.default pos)
        , dict = Dict.map fn g.dict
        }


mapPositions : List ( Int, Int ) -> (( Int, Int ) -> a -> a) -> Grid a -> Grid a
mapPositions positions fn (Grid g) =
    Grid
        { g | dict = List.foldl (\pos d -> Dict.update pos (Maybe.map (fn pos)) d) g.dict positions }


filter : (( Int, Int ) -> a -> Bool) -> Grid a -> Grid a
filter fn (Grid g) =
    Grid { g | dict = Dict.filter fn g.dict }


getByDeltasOrDefault : List ( Int, Int ) -> ( Int, Int ) -> Grid a -> List a
getByDeltasOrDefault deltas pos grid =
    deltas
        |> List.map (\delta -> atOrDefault (addPosition pos delta) grid)


getByDeltas : List ( Int, Int ) -> ( Int, Int ) -> Grid a -> List a
getByDeltas deltas pos grid =
    deltas
        |> List.filterMap (\delta -> at (addPosition pos delta) grid)


allNeighboursOrDefault : ( Int, Int ) -> Grid a -> List a
allNeighboursOrDefault =
    getByDeltasOrDefault allNeighbours_


diagonalNeighboursOrDefault : ( Int, Int ) -> Grid a -> List a
diagonalNeighboursOrDefault =
    getByDeltasOrDefault diagonalNeighbours_


orthogonalNeighboursOrDefault : ( Int, Int ) -> Grid a -> List a
orthogonalNeighboursOrDefault =
    getByDeltasOrDefault orthogonalNeighbours_


allNeighbours : ( Int, Int ) -> Grid a -> List a
allNeighbours =
    getByDeltas allNeighbours_


allNeighboursWithPositions : ( Int, Int ) -> Grid a -> List ( ( Int, Int ), a )
allNeighboursWithPositions position grid =
    allNeighbours_
        |> List.filterMap
            (\delta ->
                let
                    newPosition =
                        addPosition position delta
                in
                at newPosition grid
                    |> Maybe.map (Tuple.pair newPosition)
            )


diagonalNeighbours : ( Int, Int ) -> Grid a -> List a
diagonalNeighbours =
    getByDeltas diagonalNeighbours_


diagonalNeighboursWithPositions : ( Int, Int ) -> Grid a -> List ( ( Int, Int ), a )
diagonalNeighboursWithPositions position grid =
    diagonalNeighbours_
        |> List.filterMap
            (\delta ->
                let
                    newPosition =
                        addPosition position delta
                in
                at newPosition grid
                    |> Maybe.map (Tuple.pair newPosition)
            )


orthogonalNeighbours : ( Int, Int ) -> Grid a -> List a
orthogonalNeighbours =
    getByDeltas orthogonalNeighbours_


orthogonalNeighboursWithPositions : ( Int, Int ) -> Grid a -> List ( ( Int, Int ), a )
orthogonalNeighboursWithPositions position grid =
    orthogonalNeighbours_
        |> List.filterMap
            (\delta ->
                let
                    newPosition =
                        addPosition position delta
                in
                at newPosition grid
                    |> Maybe.map (Tuple.pair newPosition)
            )


allNeighbourPositions : ( Int, Int ) -> List ( Int, Int )
allNeighbourPositions pos =
    List.map (addPosition pos) allNeighbours_


diagonalNeighbourPositions : ( Int, Int ) -> List ( Int, Int )
diagonalNeighbourPositions pos =
    List.map (addPosition pos) diagonalNeighbours_


orthogonalNeighbourPositions : ( Int, Int ) -> List ( Int, Int )
orthogonalNeighbourPositions pos =
    List.map (addPosition pos) orthogonalNeighbours_


addPosition : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
addPosition ( dx, dy ) ( x, y ) =
    ( x + dx, y + dy )


allNeighbours_ : List ( Int, Int )
allNeighbours_ =
    [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]


diagonalNeighbours_ : List ( Int, Int )
diagonalNeighbours_ =
    [ ( -1, -1 ), ( 1, -1 ), ( -1, 1 ), ( 1, 1 ) ]


orthogonalNeighbours_ : List ( Int, Int )
orthogonalNeighbours_ =
    [ ( 0, -1 ), ( -1, 0 ), ( 1, 0 ), ( 0, 1 ) ]


all : (( Int, Int ) -> a -> Bool) -> Grid a -> Bool
all pred (Grid g) =
    dictAll pred g.dict


dictAll : (comparable -> val -> Bool) -> Dict comparable val -> Bool
dictAll pred dict =
    not <| Dict.any (\pos v -> not <| pred pos v) dict
