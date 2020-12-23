{-# LANGUAGE RecordWildCards #-}
module Year2020Day23 where

import Debug.Trace (traceShowId)
import qualified Data.List.Zipper as Z
import Data.List.Zipper (Zipper)
import Data.Function ((&))
import Data.Set (Set)
import qualified Data.Set as Set

exampleInput :: [Integer]
exampleInput = [3,8,9,1,2,5,4,6,7]

realInput :: [Integer]
realInput = [4,6,3,5,2,8,1,7,9]

fromList :: [Integer] -> Z
fromList list =
    Z { z = Z.fromList list, s = Set.fromList list }

toList :: Z -> [Integer]
toList Z{..} =
    Z.toList z 

left :: Z -> Z
left zz@Z{..} =
    if Z.beginp z then
        zz { z = Z.left $ Z.end z }
    else
        zz { z = Z.left z }

right :: Z -> Z
right zz@Z{..} =
    let r = Z.right z
    in
    if Z.endp r then
        zz { z = Z.start z }
    else
        zz { z = r }

insert :: Integer -> Z -> Z
insert n zz@Z{..} =
    zz { z = Z.insert n z, s = Set.insert n s }

delete :: Z -> Z
delete zz@Z{..} =
    let n = Z.cursor z
        del = Z.delete z
        newZ =
            if Z.endp del then
                Z.start del
            else
                del
    in zz { z = newZ, s = Set.delete n s }

current :: Z -> Integer
current Z{..} =
    Z.cursor z

data Z = Z
    { z :: Zipper Integer
    , s :: Set Integer
    }
    deriving (Show)

findDestination :: Z -> Integer -> Integer
findDestination Z{..} n =
    go $ n - 1
    where
        minS = minimum s

        go wanted 
            | wanted < minS = maximum s
            | Set.member wanted s = wanted
            | otherwise = go $ wanted - 1

repeatUntil :: (a -> a) -> (a -> Bool) -> a -> a
repeatUntil step p value =
    if p value then
        value
    else
        repeatUntil step p $ step value

move :: Z -> Z
move z =
    let
        z1 = right z
        z2 = right z1
        z3 = right z2
        (i1,i2,i3) = (current z1, current z2, current z3)
        zWithout3 = delete $ delete $ delete z1
        curr = current z
        destination = findDestination zWithout3 curr
        zOnDestination = repeatUntil right (\zz -> current zz == destination) zWithout3
        zWithPasted3 = zOnDestination & right & insert i3 & insert i2 & insert i1
    in
    zWithPasted3
    & repeatUntil right (\zz -> current zz == curr)
    & right

times :: Integer -> (a -> a) -> a -> a
times n step value =
    if traceShowId n <= 0 then
        value
    else
        times (n-1) step (step value)

afterOne1 :: [Integer] -> [Integer]
afterOne1 list =
    list
    & cycle
    & dropWhile (/= 1)
    & drop 1
    & take 8

part1 :: IO ()
part1 = do
    let final = times 100 move $ fromList realInput
    print $ afterOne1 $ toList final

afterOne2 :: [Integer] -> Integer
afterOne2 list =
    list
    & cycle
    & dropWhile (/= 1)
    & drop 1
    & take 2
    & product

fixForPart2 :: [Integer] -> [Integer]
fixForPart2 list =
    list ++ [10..1000000]

main :: IO ()
main = do
    let final = times 10000000 move $ fromList $ fixForPart2 realInput
    print $ afterOne2 $ toList final