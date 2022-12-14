{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module Day23 (main) where

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
import qualified Data.Char as Char
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified GHC.List as List2

main :: IO ()
main = 
  -- example
  realInput
  & parse
  & debugLog "parsed"
  & compute1
  & print

---------------------------------------------------------

data Instruction
  = Cpy Val Val
  | Inc Reg
  | Dec Reg
  | Jnz Val Val
  | Tgl Reg
  deriving (Show, Eq, Ord, Generic)

data Val
  = RV String
  | IV Int
  deriving (Show, Eq, Ord, Generic)

data Reg = R String
  deriving (Show, Eq, Ord, Generic)

---------------------------------------------------------

compute1 :: [Instruction] -> Int
compute1 !instrs =
  go 0 (Map.fromList [("a",7)]) instrs
  & snd
  & (! "a")

-- For part 2 I had to run the assembly with inputs 6..10, and then figure out it's:
-- A(n) = n! + 7820
--
-- compute2 :: [Instruction] -> Int
-- compute2 !instrs =
--   go 0 (Map.fromList [("a",12)]) instrs
--   & snd
--   & (! "a")

go :: Int -> Map String Int -> [Instruction] -> (Int, Map String Int)
go ip regs [] = (ip,regs)
go ip regs ixs 
  | ip >= length ixs = (ip,regs)
  | otherwise = 
  --let !_ = debugLog "go" (ip,regs,instr) in
  case instr of
    Cpy v1 (RV r) -> go (ip + 1) (Map.insert r (val v1)     regs) ixs
    Cpy _  (IV _) -> go (ip + 1) regs                             ixs -- skip
    Inc (R r)     -> go (ip + 1) (Map.adjust (+ 1)        r regs) ixs
    Dec (R r)     -> go (ip + 1) (Map.adjust (subtract 1) r regs) ixs
    Jnz v1 v2     -> if val v1 == 0 then go (ip + 1)      regs ixs
                     else                go (ip + val v2) regs ixs
    Tgl r -> 
      let pIp = ip + rr r in
      case getAt pIp ixs of
        Nothing -> go (ip + 1) regs ixs
        Just pInstr ->
          let
            toggledInstr = 
              case pInstr of
                Inc r -> Dec r
                Dec r -> Inc r
                Tgl r -> Inc r
                Jnz v1 v2 -> Cpy v1 v2
                Cpy v1 v2 -> Jnz v1 v2
          in
          go (ip + 1) regs (setAt pIp toggledInstr ixs)
  where
    instr = ixs !! ip
    val (IV n) = n
    val (RV r) = regs ! r
    rr  (R r)  = regs ! r
  

---------------------------------------------------------

parse :: String -> [Instruction]
parse input =
  input
    & lines
    & map parseInstruction

parseInstruction :: String -> Instruction
parseInstruction line =
  case words line of
    ["cpy",x,y] -> Cpy (parseVal x) (parseVal y)
    ["inc",x]   -> Inc (R x)
    ["dec",x]   -> Dec (R x)
    ["jnz",x,y] -> Jnz (parseVal x) (parseVal y)
    ["tgl",x]   -> Tgl (R x)

parseVal :: String -> Val
parseVal val
  | List2.all (\c -> c == '-' || Char.isDigit c) val = IV (read val)
  | otherwise = RV val

---------------------------------------------------------

split :: String -> String -> [String]
split pattern = filter (not . null) . split'
  where
    split' src = case listToMaybe (src =~~ pattern) of
        Nothing     -> [src]
        Just (o, l) ->
            let (before, tmp) = splitAt o src
            in before : split' (drop l tmp)

debugLog :: Show a => String -> a -> a
debugLog label x =
  trace (label ++ ": " ++ show x) x

debugLogFn :: Show b => String -> (a -> b) -> a -> a
debugLogFn label fn x =
  trace (label ++ ": " ++ show (fn x)) x

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = List.sortBy (\x y -> compare (f x) (f y))

setAt :: Int -> a -> [a] -> [a]
setAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_:xs) = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []

getAt :: Int -> [a] -> Maybe a
getAt i xs 
    | i < 0     = Nothing
    | otherwise = go i xs
  where
    go :: Int -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing

---------------------------------------------------------

example :: String
example =
  [r|cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a|]

realInput :: String
realInput =
  [r|cpy a b
dec b
cpy a d
cpy 0 a
cpy b c
inc a
dec c
jnz c -2
dec d
jnz d -5
dec b
cpy b c
cpy c d
dec d
inc c
jnz d -2
tgl c
cpy -16 c
jnz 1 c
cpy 85 c
jnz 92 d
inc a
inc d
jnz d -2
inc c
jnz c -5|]
