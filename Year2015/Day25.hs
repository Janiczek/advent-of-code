module Year2015Day25 where

row :: Int
col :: Int
(row, col) =
  (2978, 3083)

-- 1st "row"
-- (1,1)
-- sum always 1+1

-- 4th "row"
-- (4,1) (3,2) (2,3) (1,4)
-- sum always 4+1

-- 5th "row"
-- (5,1) (4,2) (3,3) (2,4) (1,5)
-- sum always 5+1

-- 4th number in the 5th row = (_, 4)

triangleRow :: (Int, Int) -> Int
triangleRow (r, c) =
  r + c - 1

triangularNumber :: Int -> Int
triangularNumber n =
  (n * (n + 1)) `div` 2

numberOfIterations :: (Int, Int) -> Int
numberOfIterations (r, c) =
  triangularNumber (triangleRow (r, c) - 1) -- one less row, the last one will not be necessarily complete
    + c -- unfinished last row

initNumber :: Int
initNumber =
  20151125

step :: Int -> Int
step n =
  (n * 252533) `rem` 33554393

numbers :: [Int]
numbers =
    iterate step initNumber

nthNumber :: Int -> Int
nthNumber n =
    -- 1-based!
    numbers !! (n - 1)

numberAtRC :: (Int,Int) -> Int
numberAtRC (r,c) =
    nthNumber $ numberOfIterations (r,c)

main :: IO ()
main = do
  print (row,col)
  print $ numberOfIterations (row,col)
  print $ numberAtRC (row,col)
