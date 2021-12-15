module AoC.Day15

let exampleInput: int64 list = [0L;3L;6L]
let realInput: int64 list = [14L;1L;17L;0L;3L;20L]

let generic (stop: int64) (input: int64 list): int64 =
    let rec go (i: int64) (last: int64) (lasts: Map<int64,int64>) (prevs: Map<int64,int64>) =
        if i = stop then
            last
        else if i <= int64 input.Length then
            let current = input.[int (i-1L)]
            let newPrevs =
                match lasts.TryFind current with
                | None -> prevs
                | Some lastI -> prevs.Add(current,lastI)
            let newLasts = lasts.Add(current,i)
            go (i+1L) current newLasts newPrevs
        else
            let current =
                match prevs.TryFind last with
                | None -> 0L
                | Some p -> i - p - 1L
            let newPrevs =
                match lasts.TryFind current with
                | None -> prevs
                | Some lastI -> prevs.Add(current,lastI)
            let newLasts = lasts.Add(current,i)
            go (i+1L) current newLasts newPrevs
    go 1L 0L Map.empty Map.empty
    
    
let part1 = generic 2020L

let main = part1 realInput
