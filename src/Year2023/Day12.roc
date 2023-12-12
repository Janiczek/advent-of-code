app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "../../input202312.txt" as realInput : Str
    ]
    provides [main] to pf

crashResult : Result a *, Str -> a
crashResult = \result,label ->
    when result is
        Ok a -> a
        Err _ -> crash label

testInput =
    """
    ???.### 1,1,3
    .??..??...?##. 1,1,3
    ?#?#?#?#?#?#?#? 1,3,1,6
    ????.#...#... 4,1,1
    ????.######..#####. 1,6,5
    ?###???????? 3,2,1
    """

main = part1 realInput
#main = part2 realInput
#main = part1 testInput
#main = part2 testInput

pipeDbg = \x ->
    dbg x
    x

### PARSE ###

parse : Str -> List Line
parse = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map parseLine

parseLine : Str -> Line
parseLine = \input ->
    when input |> Str.split " " is
        [springs,lengths] ->
            {
                springs: Str.graphemes springs,
                lengths: 
                    lengths
                    |> Str.split ","
                    |> List.mapTry Str.toNat
                    |> crashResult "parse lengths"
            }

        _ -> crash "parseLine - weird input"

Line: {
    springs: List Str,
    lengths: List Nat,
}

### PART 1 ###

part1 = \input ->
    input
    |> parse
    |> List.map countPossibilities
    |> List.sum
    |> Inspect.toStr
    |> Stdout.line

countPossibilities = \line ->
    unknowns = line.springs |> List.countIf (\s -> s == "?")
    allPossibilities = 
        ["#", "."]
        |> List.repeat unknowns
        |> cartesianProduct
    allPossibilities
    |> List.countIf (\possibility -> agreesWith line possibility)

agreesWith : Line,List Str -> Bool
agreesWith = \line,possibility ->
    newSprings = combine line.springs possibility
    countSprings newSprings == line.lengths

combine : List Str,List Str -> List Str
combine = \haystack,todos ->
    todos
    |> List.walk
        haystack
        (\acc,todo ->
            res = 
                acc
                |> List.splitFirst "?"
                |> crashResult "combine - no more ?"

            List.join 
                [ 
                    res.before,
                    [todo],
                    res.after,
                ]
        )

expect combine (Str.graphemes "?..#.??#?") (Str.graphemes "ABCD")
        == Str.graphemes "A..#.BC#D"

countSprings : List Str -> List Nat
countSprings = \springs ->
    springs
    |> group
    |> List.dropIf (\xs -> List.first xs == Ok ".")
    |> List.map List.len

expect countSprings (Str.graphemes ".##.#.####") == [2,1,4]
expect countSprings (Str.graphemes "###.#.###.") == [3,1,3]

group = \items ->
    items
    |> List.walk
        {full:[],group:[],last:NoItem}
        (\acc,x ->
            when acc.last is
                NoItem ->
                    {acc & 
                        group: acc.group |> List.append x,
                        last: Item x
                    }
                Item prev ->
                    if prev == x then
                        {acc & 
                            group: acc.group |> List.append x,
                            last: Item x
                        }
                    else
                        { 
                            full: acc.full |> List.append acc.group,
                            group: [x],
                            last: Item x
                        }
        )
        |> (\final -> final.full |> List.append final.group)

expect group [A,A,B,B,B,C,D,A,B] == [[A,A],[B,B,B],[C],[D],[A],[B]]

cartesianProduct : List (List a) -> List (List a)
                    where a implements Inspect.Inspect
cartesianProduct = \ll ->
    when ll is
        [] ->
            [ [] ]

        [.. as xss, xs] ->
            map2Cartesian 
                (cartesianProduct xss) 
                xs
                List.append

expect cartesianProduct [[A,B],[C,D,E]] == [[A,C],[A,D],[A,E],[B,C],[B,D],[B,E]]

map2Cartesian : List a,List b,(a,b -> c) -> List c
map2Cartesian = \la,lb,f ->
    la
    |> List.joinMap (\a ->
        lb
        |> List.joinMap (\b -> 
            [f a b]
        )
    )
