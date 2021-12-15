import qualified Data.Map as Map

inputLen = length input
initIndexes input = Map.fromList $ zip input [1..] 

get n = go inputLen (input !! (inputLen - 1)) (initIndexes input)
  where
    go lastI last indexes =
      if lastI == n then
        last
      else
        let current =
              case Map.lookup last indexes of
                Just n -> lastI - n
                _ -> 0
            newIndexes = Map.insert last lastI indexes
        in
        go (lastI+1) current newIndexes


input = [14,1,17,0,3,20]
maxn = 30000000
main = print $ get maxn
