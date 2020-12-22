module Year2020Day22 (main) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

playerCards :: [Int]
playerCards = [43,36,13,11,20,25,37,38,4,18,1,8,27,23,7,22,10,5,50,40,45,26,15,32,33]

crabCards :: [Int]
crabCards = [21,29,12,28,46,9,44,6,16,39,19,24,17,14,47,48,42,34,31,3,41,35,2,30,49]

play1 :: ([Int],[Int]) -> [Int]
play1 (p1,p2) =
    case (p1,p2) of
        ([],_) -> p2
        (_,[]) -> p1
        (a:as,b:bs) -> 
            if a > b then
                play1 (as ++ [a,b], bs)
            else if b > a then
                play1 (as, bs ++ [b,a])
            else
                error "draw!"

type Memo = Map ([Int],[Int]) (Bool, [Int])
type Players = ([Int],[Int])

play2 :: Players -> [Int]
play2 (p1,p2) =
    cards
    where 
        (_,_,cards) = go Map.empty Set.empty (p1,p2) (p1,p2)
        go :: Memo -> Set Players -> Players -> Players -> (Bool, Memo, [Int])
        go memo seen (p1,p2) (orig1, orig2) =
            if Set.member (p1,p2) seen then
                (True, memo, p1)
            else 
              case Map.lookup (p1,p2) memo of
                Nothing -> 
                  case (p1,p2) of
                      ([],_) -> (False, Map.insert (orig1,orig2) (False,p2) memo, p2)
                      (_,[]) -> (True, Map.insert (orig1,orig2) (True,p1) memo, p1)
                      (a:as,b:bs) ->
                          let (resultP1Won, (resultAs, resultBs)) = 
                                  if length as >= a && length bs >= b then
                                      let newPlayers = (take a as, take b bs)
                                          (p1Won,_, _) = go memo Set.empty newPlayers newPlayers
                                      in
                                      ( p1Won
                                      , if p1Won then 
                                           (as ++ [a,b], bs)
                                        else 
                                           (as, bs ++ [b,a])
                                      )
                                  else if a > b then
                                     (True, (as ++ [a,b], bs))
                                  else if b > a then
                                      (False, (as, bs ++ [b,a]))
                                  else
                                      error "draw!"
                          in
                          go 
                            (Map.insert (p1,p2) (resultP1Won, if resultP1Won then resultAs else resultBs) memo) 
                            (Set.insert (p1,p2) seen)
                            (resultAs, resultBs)
                            (orig1,orig2)
                Just (resultP1Won, winCards) ->
                  (resultP1Won, memo, winCards)






eval :: [Int] -> Int
eval list =
    sum $ zipWith (*) list [len,len-1..1]
    where
        len = length list

main :: IO ()
main = 
    --print $ eval $ play1 (playerCards, crabCards)
    print $ eval $ play2 (playerCards, crabCards)
    --print $ eval $ play2 ([9,2,6,3,1],[5,8,4,7,10])
