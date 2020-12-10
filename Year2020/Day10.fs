module AoC.Day10

open System.Collections.Generic
open FSharpPlus

let exampleInput = "16
10
15
5
1
11
7
19
6
12
4"

let example2Input = "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"

let realInput = "56
139
42
28
3
87
142
57
147
6
117
95
2
112
107
54
146
104
40
26
136
127
111
47
8
24
13
92
18
130
141
37
81
148
31
62
50
80
91
33
77
1
96
100
9
120
27
97
60
102
25
83
55
118
19
113
49
133
14
119
88
124
110
145
65
21
7
74
72
61
103
20
41
53
32
44
10
34
121
114
67
69
66
82
101
68
84
48
73
17
43
140"

let parse (input: string): Set<int> =
    input.Split "\n" |> Array.map int32 |> Set.ofArray

let maxJoltage (input: Set<int>): int = input |> Set.maxElement |> (+) 3

let rec findPath (todo: int list list) (goal: int) (all: Set<int>): int list =
    match todo with
    | [] -> failwith "empty queue!"
    | pathSoFar :: restOfTodo ->
        let current = List.head pathSoFar
        if current = goal then
            pathSoFar
        else
            let usable =
                all
                |> Set.filter (fun n -> n - current > 0 && n - current < 4)

            let newTodo: int list =
                usable
                |> Set.minElement
                |> (fun lowestUsableJolt -> lowestUsableJolt :: pathSoFar)

            findPath (newTodo :: restOfTodo) goal all



let part1 (input: Set<int>): int =
    let max = maxJoltage input
    let inputWithEnd: Set<int> = input |> Set.add max
    let path: int list = findPath [ [ 0 ] ] max inputWithEnd
    printfn "found path %A" path

    let diffs =
        List.pairwise path
        |> List.map (fun (a, b) -> a - b)

    let oneJolts =
        diffs
        |> List.filter (fun x -> x = 1)
        |> List.length

    let threeJolts =
        diffs
        |> List.filter (fun x -> x = 3)
        |> List.length

    oneJolts * threeJolts

// http://www.fssnip.net/8P/title/Memoization-for-dynamic-programming
let memoize f =
    let cache = new Dictionary<_, _>()
    (fun x ->
        let succ, v = cache.TryGetValue(x)
        if succ then
            v
        else
            let v = f (x)
            cache.Add(x, v)
            v)

let part2 (input: Set<int>): int64 =
    let max = maxJoltage input
    let inputWithEnds: Set<int> = input |> Set.add 0 |> Set.add max

    let rec ways =
        memoize (fun n ->
            if not (Set.contains n inputWithEnds)
            then 0L
            else if n < 2
            then 1L
            else ways (n - 1) + ways (n - 2) + ways (n - 3))

    ways max


let main = realInput |> parse |> part2
