module Grid.Zipper exposing
    ( Zipper
    , current
    , currentFocus
    , currentOrDefault
    , doNTimes
    , doUntil
    , doWhile
    , duplicate
    , extend
    , extract
    , filter
    , fromGrid
    , fromGridAtOrigin
    , goDown
    , goLeft
    , goRight
    , goTo
    , goUp
    , gridFn
    , map
    , mapFocus
    , set
    , toGrid
    , withCurrentOrDefault
    )

import Grid exposing (Grid)


type Zipper a
    = Zipper
        { grid : Grid a
        , focus : ( Int, Int )
        }


fromGrid : ( Int, Int ) -> Grid a -> Zipper a
fromGrid focus grid =
    Zipper
        { grid = grid
        , focus = focus
        }


fromGridAtOrigin : Grid a -> Zipper a
fromGridAtOrigin =
    fromGrid ( 0, 0 )


current : Zipper a -> Maybe a
current (Zipper z) =
    Grid.at z.focus z.grid


currentOrDefault : Zipper a -> a
currentOrDefault =
    extract


currentFocus : Zipper a -> ( Int, Int )
currentFocus (Zipper z) =
    z.focus


toGrid : Zipper a -> Grid a
toGrid (Zipper z) =
    z.grid


extract : Zipper a -> a
extract (Zipper z) =
    Grid.atOrDefault z.focus z.grid


duplicate : Zipper a -> Zipper (Zipper a)
duplicate (Zipper z) =
    Zipper
        { grid = Grid.map (\pos a -> fromGrid pos z.grid) z.grid
        , focus = z.focus
        }


extend : (Zipper a -> b) -> Zipper a -> Zipper b
extend fn =
    map (\_ zipper -> fn zipper) << duplicate


map : (( Int, Int ) -> a -> b) -> Zipper a -> Zipper b
map fn (Zipper z) =
    Zipper
        { grid = Grid.map fn z.grid
        , focus = z.focus
        }


goTo : ( Int, Int ) -> Zipper a -> Zipper a
goTo focus (Zipper z) =
    Zipper { z | focus = focus }


mapFocus : (( Int, Int ) -> ( Int, Int )) -> Zipper a -> Zipper a
mapFocus fn (Zipper z) =
    Zipper { z | focus = fn z.focus }


goLeft : Zipper a -> Zipper a
goLeft =
    mapFocus (Grid.addPosition ( -1, 0 ))


goRight : Zipper a -> Zipper a
goRight =
    mapFocus (Grid.addPosition ( 1, 0 ))


goUp : Zipper a -> Zipper a
goUp =
    mapFocus (Grid.addPosition ( 0, -1 ))


goDown : Zipper a -> Zipper a
goDown =
    mapFocus (Grid.addPosition ( 0, 1 ))


doNTimes : Int -> (Zipper a -> Zipper a) -> Zipper a -> Zipper a
doNTimes n fn zipper =
    if n <= 0 then
        zipper

    else
        doNTimes (n - 1) fn (fn zipper)


doWhile : (Zipper a -> Bool) -> (Zipper a -> Zipper a) -> Zipper a -> Zipper a
doWhile pred fn zipper =
    if pred zipper then
        doWhile pred fn (fn zipper)

    else
        zipper


doUntil : (Zipper a -> Bool) -> (Zipper a -> Zipper a) -> Zipper a -> Zipper a
doUntil pred fn zipper =
    doWhile (not << pred) fn zipper


set : a -> Zipper a -> Zipper a
set val (Zipper z) =
    Zipper { z | grid = Grid.set z.focus val z.grid }


withCurrent : (Zipper a -> b) -> Zipper a -> ( Maybe a, b )
withCurrent fn zipper =
    ( current zipper
    , fn zipper
    )


withCurrentOrDefault : (Zipper a -> b) -> Zipper a -> ( a, b )
withCurrentOrDefault fn zipper =
    ( currentOrDefault zipper
    , fn zipper
    )


filter : (( Int, Int ) -> a -> Bool) -> Zipper a -> Zipper a
filter fn (Zipper z) =
    Zipper { z | grid = Grid.filter fn z.grid }


gridFn : (( Int, Int ) -> Grid a -> b) -> Zipper a -> b
gridFn fn (Zipper z) =
    fn z.focus z.grid
