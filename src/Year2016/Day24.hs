{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module Day24 (main) where

import Data.Function ((&))
import Data.Graph.AStar (aStar)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Map (Map, (!))
import Data.Map.Internal.Debug as MapD
import Data.Maybe (listToMaybe)
import Debug.Trace
import GHC.Generics (Generic)
import Text.RawString.QQ (r)
import Text.Regex.TDFA ((=~~))
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

main :: IO ()
main = print $ length $ pathInGraph2 m
  where
    --m = parseMap example
    m = parseMap realInput

pathInGraph1 :: Map Pos Entity -> [Pos]
pathInGraph1 m =
  perms
    & map (\p -> zero : p)
    & sortWith (cost paths)
    & head
    & pairs
    & concatMap (\pair -> paths ! pair)
  where
    zero = findMandatory m 0
    paths = allPaths m
    perms = List.permutations $ findMsWithoutZero m

pathInGraph2 :: Map Pos Entity -> [Pos]
pathInGraph2 m =
  perms
    & map (\p -> zero : p ++ [zero])
    & sortWith (cost paths)
    & head
    & pairs
    & concatMap (\pair -> paths ! pair)
  where
    zero = findMandatory m 0
    paths = allPaths m
    perms = List.permutations $ findMsWithoutZero m
    
cost :: Map (Pos,Pos) [Pos] -> [Pos] -> Int
cost paths path =
  path
  & pairs
  & map (\pair -> paths ! pair)
  & map length
  & sum

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = List.sortBy (\x y -> compare (f x) (f y))

allPaths :: Map Pos Entity -> Map (Pos,Pos) [Pos]
allPaths m =
  [ (a,b) | a <- ms, b <- ms ]
    & Maybe.mapMaybe (\(a,b) -> findPath m a b & fmap (\p -> ((a,b),p)))
    & Map.fromList
    where
      ms = findMs m
      mm = mandatorys m

findMs :: Map Pos Entity -> [Pos]
findMs m =
  m
  & Map.filter (\case
    Mandatory _ -> True
    _ -> False
  )
  & Map.keys

findMsWithoutZero :: Map Pos Entity -> [Pos]
findMsWithoutZero m =
  m
  & Map.filter (\case
    Mandatory 0 -> False
    Mandatory _ -> True
    _ -> False
  )
  & Map.keys

findPath :: Map Pos Entity -> Pos -> Pos -> Maybe [Pos]
findPath m start goal =
  aStar 
    (neighboursOf m)
    (\_ _ -> 1)
    (distanceHeuristic goal)
    (== goal)
    start

findMandatory :: Map Pos Entity -> Int -> Pos
findMandatory m n =
  m
  & Map.toList
  & Maybe.mapMaybe (\(pos,ent) ->
    case ent of
      Mandatory x -> if x == n then Just pos else Nothing
      _ -> Nothing
  )
  & head

neighboursOf :: Map Pos Entity -> Pos -> HashSet Pos
neighboursOf map_ pos =
  pos
    & orthogonalNeighbours map_
    & filter (\p -> Map.lookup p map_ /= Just Wall)
    & HashSet.fromList

orthogonalNeighbours :: Map Pos Entity -> Pos -> [Pos]
orthogonalNeighbours map_ coords =
  [(-1,0),(1,0),(0,-1),(0,1)]
    & map Pos
    & map (coords +)
    & filter (\pos -> Map.member pos map_)

distanceHeuristic :: Pos -> Pos -> Int
distanceHeuristic (Pos (x1,y1)) (Pos (x2,y2)) =
  abs (x1 - x2) + abs (y1 - y2)


example :: String
example =
  [r|###########
#0.1.....2#
#.#######.#
#4.......3#
###########|]

parseMap :: String -> Map Pos Entity
parseMap input =
  input
    & splitAll "\n"
    & zip [0..]
    & concatMap
     (\(y,cs) -> 
      cs
        & zip [0..] 
        & map (\(x,c) -> (Pos (x,y),parseEntity c))
    )
    & Map.fromList

parseEntity :: Char -> Entity
parseEntity = \case
    '#' -> Wall
    '.' -> Open
    c   -> Mandatory (read [c])

data Entity
  = Wall
  | Open
  | Mandatory Int
  deriving (Show, Eq, Ord, Generic)


newtype Pos = Pos (Int,Int)
  deriving (Show, Eq, Ord, Generic)

instance Hashable Pos
instance Num Pos where
  Pos (x1,y1) + Pos (x2,y2) = Pos (x1+x2,y1+y2)
  Pos (x1,y1) * Pos (x2,y2) = Pos (x1*x2,y1*y2)
  abs (Pos (x,y)) = Pos (abs x, abs y)
  signum (Pos (x,y)) = Pos (signum x, signum y)
  fromInteger n = Pos (fromInteger n,fromInteger n)
  negate (Pos (x,y)) = Pos (negate x, negate y)

mandatorys :: Ord v => Map k v -> Map v k
mandatorys m =
  m
  & Map.toList
  & map (\(a,b) -> (b,a))
  & Map.fromList

splitAll :: String -> String -> [String]
splitAll pattern = filter (not . null) . splitAll'
  where
    splitAll' src = case listToMaybe (src =~~ pattern) of
        Nothing     -> [src]
        Just (o, l) ->
            let (before, tmp) = splitAt o src
            in before : splitAll' (drop l tmp)

debugLog :: Show a => String -> a -> a
debugLog label x =
  trace (label ++ ": " ++ show x) x

debugLogFn :: Show b => String -> (a -> b) -> a -> a
debugLogFn label fn x =
  trace (label ++ ": " ++ show (fn x)) x

realInput :: String
realInput =
  [r|#########################################################################################################################################################################################
#.#.......#.#.#.....#.#.......#.................#.......#.#.....#.....#...#...#.......#...#...........#.#.....#.............#.........#.............#.........#.....#.#.............#...#
#.#####.#.#.#.#.#.#.#.#.#.###.#.#.###.#.#.###.###.#.#.#.#.#.#####.#.#.###.#.#.###.#.#.###.###.#.###.###.###.#.#.###.#.#.#.#.#.#.###.#.###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.###.#####.#.###.#
#.....#.............#...#....6#.....#.....#.#...#.....#...#.........#.......#...#.#.....#.....#.#...#...#.....#.#.......#.........#...#...#.#.#.......#.........#.....#...#.#.#.#.....#.#
#####.#.###.###.#.#.#.#####.#.#####.#.#####.#.#######.#.#.#.#.#.#####.#####.#.#########.#.###.#.#.#.#.#.#.#.#######.#.#.#####.###.###.###.#.#.###.#.#.#.#.#.#.#.#.###########.#.#.#####.#
#.....#.......#.#.....#.......#.#...#.#.#.#...........#.....#.#.#...#.#...#.#.#.....#...#.....#.....#...............#.......#.#.........#.#...........#.....#...#.......#.#...#.#.....#.#
###.#.#.#.#####.#####.#.#.#.#.#.#.#.#.#.#.#########.#.#########.#.#.###.###.###.###.#.#.#.#.#.#.#.#######.#.#.###.#.###.#.#.#.#.#.#.#######.#.#.#.###.#.#.#.###.###.#.#.#.#.#.###.#.#####
#...#...#.#.....#.............#...#...#...#.#.......#.#.#...#...#.#.#...#.....#.#.#.....#.#...#.#.......#.#.#...#.......#.....#.....#.......#...#...#.#...#.....#.#.......#.#.#...#...#.#
#.#.#####.#.###.###.#.#########.#.#.#.###.#.#.#.#.#.#.#.#.#.#.#.#.#.###.#.#.#.###.#.#.#.#.#######.#.#.###.#.###.###.#.#.#.#####.#####.#####.#.#.#.#.#.###.#.#.###.#.#####.#.#####.#.###.#
#...#.#...#.....#.#.........#.#.....#...#.....#...#...#...#...#...#.........#.......#...#.#.....#...#.#...#.#.......#.........#...#.#...#...#2..#.#.#.#...#.......#...#.#...........#...#
#.###.#####.#####.#.#.###.###.#.###.#.#.#.#.###.#.#.###.#.#.#.###.#.#.#####.#.#####.#.###.#.#.#.#.###.#.#.#.###.#.#.#.#####.#.#.#.#.#.###.#####.#.#.#.#.#.#####.###.#.#.#.#.#.#.#####.#.#
#...#.#.#4#.#.........#.......#...........#.#...#...#...#.........#.......#.....#...#...#.#...#...#...#.#...#.........#.....#...#.....#.........#...#...............#.#.#...#.........#.#
#######.#.#.###.###.###.###.#.#####.#####.#.#.###.#.#.#########.#####.#.#####.#.#.#.###.#.#.###.#.#.#.#.#.#.#.#####.#.#####.#.#.#.#.#.#.#.#.#.###.#.#######.#####.#.#.#.#.#.#.###.#.#####
#.........#.#.....#.....#...................#.....#...#.........#.....#.#.....#.#...........#...#...#.........#...#...#.....#...#.#.#.#.....#.......#.#3....#.......#.....#.#.#.....#...#
#.#####.###.#.###.#####.#.###.###.###.#.#####.#.#.###.###.#.###.#.#.#.###.#.###.#.###.#.#####.#.###.###.#######.#.#.#.#.###.#.#.#.#########.#.###.###.###.#.#.###.#.#######.###.#.#.#.#.#
#...#...#.........#...#.......#...#...#...#.........#.....#.....#.....#...#.......#...#...#...#.#.#.#...#.................#.............................#...#.....#.....#.#.....#.....#.#
#.#.#.#.#####.###.#.#.#.#.###.###.###.#.###.#.#.#####.###.#.###.#.#.###.#.#.#####.###.#.#.###.#.#.#.###.#.#.###.#####.#.###.###.#.#.#.#.#.#.###.#.#.#.#.#.###.###.#.#.#.#.#####.#####.#.#
#...#.......#...#.....#...#.#.#.#.#...#.#...#.....#.#.#.....#.....#.......#.#.......#.........#.....#...#...#...#.#.........#.#.#.....#.#...#...#.....#...#.......#.........#.....#...#.#
###.#.#.#.#.#####.###.#####.#.#.#.#.###.#####.###.#.#.#.#####.#.#.#.#.#.#.###.#.#.###.#.#.###.#.#.###.#.#.###.###.#.#.#.#.#.#.#.#.#####.#.#.#.###.#.#.#.###.###.###.###.###.#.#.#####.#.#
#7#.......#...#...#...#...#.......#.#.........#.#.......#.....#...#.....#.#...#...#...#.#.#...#.#.......#.....#.#.#.....#...#...#...#.....#.....#.#.#.#.#...#...#...#.#.......#.....#...#
#.#.#.#.#.#.#######.#.#.#.#####.#.###.###.#.###.#.###.#.###.#.#######.###.#.#.#.#.#####.###.###.#####.#.#####.#.###.#####.#.#.#.###.#.#######.#.#.###.#.###.#####.#.#.#.#.###.###.###.#.#
#.......#...#.#.....#.#.....#.#...#.......#...#.......#.#.....#.........#.#.#.....#.........#.#.#...............#...#.....#...#.#...#.......#.#...........#...#.#.#.#.#.#.....#...#.....#
#.#.#.#######.#.###.#.#.###.#.#.#.#.#####.#.#.#.#.#.#.#.#.#.#.#.#######.#####.###.#.#####.###.#####.#.#.###.#.###.#.#.###.#.###.#.#.#.#.#.###.###.#.###.#.#.#.#.#.#.###.#.#.#.#.#.#.#.#.#
#.....#.#...#.#.....#...........#...#.#.....#.#.......#.....#.#...#...........#.....#.........#...#.#.#.#.#.#.....#...#.#.............#.....#.#.#.#.#.#.#.#.#.....#.#.#.#...........#...#
#####.#.###.#.#.#.#.#.###.#.#.#.###.#.#.#######.#####.###.#.#.#.#.#.#.#.#.#.###.#.#####.#.#######.#.#####.#.#.#########.#########.#.#.###.###.#.#####.#.###.###.#.#.#.#.#.#.#.#######.#.#
#.....#...#.#.......#...........#...#.#.....#.#.......#.#...#.#...#.......#...#.......#...#.........#...#.........................#...#...#...#.#.#...#.#.........#...#...#...#...#...#.#
#.#.#.#.#.#.#####.#.#.#########.#.#####.#.#.#.#.#.###.#.#.#.#.###.#.#.#.#.#.#.###.#.#######.###.#.#.#.#######.#.###.#####.###.#.#.#####.#.#.#####.#######.#.#.###.###.#.#.#.#.#.###.###.#
#.#...#...........#.#.......#...#.#.............#.......#...#.....#.#.....#...#.....#.......#...#.#.#...#...#.#...#.#.....#...#...#...#.....#.....#...#.....#.......#.....#.#.#.#0#.....#
###.#.#.#.#.#.#######.#.###.###.#.#.###.#.#######.#.###.#########.#.#.#.#.###.#.###.###########.#.###.###.#.#.###.#.#.###.#.#.###.###.#.###.#.###########.###.#.###.#####.#.#.#.#.#.#.#.#
#.#...#.#.#...#.........#...#.......#.#.................#.#...........#.....#.......#.....#.#.#.#.#...#.....#...#...#.#.........#.#.....#.#...#.....#.....#.#...#.#...#.......#.#.....#.#
#.###.#.#.#.#.#.#.#.#.#.#.#.#.###.#.#.#.#.#####.#.#####.#.###.#.#.###.#.###.###.###.#.#####.#.#.#######.#.#####.###.###.#######.###.#####.#.#.#.#.#.#.#.#.#.###.#.#.#.#.#.###.#.#.#.#.#.#
#.#...#.........#.#...#...........#.#.....#5........#...#.....#.......#.#...#.......#.........#.......#.........#.#.#.......#...#.#...#.#.#.....#...#.#.#.#.......#.#.....#...#.#.....#.#
###.###.#.#####.###.###.###.#.#.###.#.#.#####.###.#.#.#.#.###.#####.#.#.#.#.#.#.#.###.#.###.#.###.#.#.#.#.###.#.#.#.#######.#.###.#.#.#.#.###.###.#.#.#.#.#.#######.#.#.#.#####.###.#.#.#
#...#...........#...#...#.#.......#.....#.#.#.#...............#.....#.....#...#.#.#...#.#...#.#.....#...#.#...#...#.#.#.......#...#.#...#.......#...#.......#.......#.#...#.#.....#.....#
#.#.#.###.#.#.#######.###.#####.#.#######.#.###.#######.#.###.###.#.#.#####.#.###.#.#.#.#.###.###.#.#.#.###.#.#.###.#.###.#########.###.#.#.#####.#.#####.###.#.#.#.###.#.#.#####.#######
#...#.................#.....#.....#...#...#.#...#...#.....#.......#.#...#...#.....#.....#.........#...#.....#.........#...#.....#...#.......#1..#.#.#.#...#...#.#.......#.#...#.......#.#
#.#.#.#####.#.#.###.###.###.#####.#.#.#####.#.#.#.#####.#.###.###.#.###.#.#.#.###.#.#.#.#.#.#.#.#.#.#.#.###.#######.#.#.#####.###.#######.#.#.#.#.#.#.#.###.#.#.#.#####.#.#######.#.#.#.#
#.....#.....#.#...............#.#...#.....#.................#...#.#...#.#...#.....#.#...#.........#...........#.#.......#.....#.#.....#.....#.......#...........#...#.....#.#.......#.#.#
#.#.#.#.#.#.#.#######.###.###.#.#.#.#.#.#.###.###.#.#.###.#.#.#.#.#.#.#####.#.#.#.#.###.#.#.#######.#.#.#.###.#.#.#.#.###.#.#.#.#.#.#.#.#####.#.#.#.#.#####.#####.###.###.#.#.#.#.#.#.#.#
#...#.#...#.#.......#.......#.....#...#.......#...#...#...#.....#.#.....#...#.#...#.....#.#.........#.#.#.....#.....#...#.........#.#.#.......#.........#...#.....#.#...#.#...#.....#...#
#########################################################################################################################################################################################|]
