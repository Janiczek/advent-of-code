{-# LANGUAGE TupleSections #-}

module Year2020Day17 (main) where

import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Function ((&))

go1 :: Int -> Set (Int,Int,Int) -> Int
go1 0 active = Set.size active
go1 turnsToDo active =
    go1 (turnsToDo - 1) newActive
    where
        d :: [Int]
        d = [-1,0,1]

        neighboursDelta :: [(Int,Int,Int)] 
        neighboursDelta =
            [(x,y,z) | x <- d, y <- d, z <- d, (x,y,z) /= (0,0,0)]

        neighbours :: (Int,Int,Int) -> [(Int,Int,Int)]
        neighbours (x,y,z) =
            [(x+dx,y+dy,z+dz) | (dx,dy,dz) <- neighboursDelta]

        aliveNeighbours :: (Int,Int,Int) -> Int
        aliveNeighbours cell =
            neighbours cell
            & Set.fromList 
            & Set.intersection active
            & Set.size
            
        isAlive :: (Bool,(Int,Int,Int)) -> Bool
        isAlive (wasAlive,cell) =
            n == 3 || (wasAlive && n == 2)
            where 
                n = aliveNeighbours cell

        newActive :: Set (Int,Int,Int)
        newActive = 
            active
            & Set.toList
            & concatMap (\cell -> (True,cell) : fmap (False,) (neighbours cell))
            & Set.fromList
            & Set.filter isAlive
            & Set.map snd

go2 :: Int -> Set (Int,Int,Int,Int) -> Int
go2 0 active = Set.size active
go2 turnsToDo active =
    go2 (turnsToDo - 1) newActive
    where
        d :: [Int]
        d = [-1,0,1]

        neighboursDelta :: [(Int,Int,Int,Int)] 
        neighboursDelta =
            [(x,y,z,w) | x <- d, y <- d, z <- d, w <-d, (x,y,z,w) /= (0,0,0,0)]

        neighbours :: (Int,Int,Int,Int) -> [(Int,Int,Int,Int)]
        neighbours (x,y,z,w) =
            [(x+dx,y+dy,z+dz,w+dw) | (dx,dy,dz,dw) <- neighboursDelta]

        aliveNeighbours :: (Int,Int,Int,Int) -> Int
        aliveNeighbours cell =
            neighbours cell
            & Set.fromList 
            & Set.intersection active
            & Set.size
            
        isAlive :: (Bool,(Int,Int,Int,Int)) -> Bool
        isAlive (wasAlive,cell) =
            n == 3 || (wasAlive && n == 2)
            where 
                n = aliveNeighbours cell

        newActive :: Set (Int,Int,Int,Int)
        newActive = 
            active
            & Set.toList
            & concatMap (\cell -> (True,cell) : fmap (False,) (neighbours cell))
            & Set.fromList
            & Set.filter isAlive
            & Set.map snd

input :: String
input = "###..#..\n\
\.#######\n\
\#####...\n\
\#..##.#.\n\
\###..##.\n\
\##...#..\n\
\..#...#.\n\
\.#....##"

testInput0 :: Set (Int,Int)
testInput0 = Set.fromList [(1,0),(2,1),(0,2),(1,2),(2,2)]

testInput1 :: Set (Int,Int,Int)
testInput1 = testInput0 & Set.map (\(x,y) -> (x,y,0))

testInput2 :: Set (Int,Int,Int,Int)
testInput2 = testInput0 & Set.map (\(x,y) -> (x,y,0,0))

parse0 :: String -> Set (Int,Int)
parse0 input =
    input
    & lines
    & map (zip [0..])
    & zip [0..]
    & concatMap (\(y,xs) -> map (\(x,char) -> (x,y,char)) xs)
    & mapMaybe (\(x,y,char) -> if char == '#' then Just (x,y) else Nothing)
    & Set.fromList

parse1 :: String -> Set (Int,Int,Int)
parse1 input =
    parse0 input
    & Set.map (\(x,y) -> (x,y,0))

parse2 :: String -> Set (Int,Int,Int,Int)
parse2 input =
    parse0 input
    & Set.map (\(x,y) -> (x,y,0,0))

main :: IO ()
main = 
    --print $ go1 6 (parse1 input)
    print $ go2 6 (parse2 input)
    