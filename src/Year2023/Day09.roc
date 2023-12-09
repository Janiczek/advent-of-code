app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "../../input202309.txt" as realInput : Str
    ]
    provides [main] to pf


#main = part1 realInput
main = part2 realInput
#main = part1 testInput
#main = part2 testInput

parse : Str -> List (List I64)
parse = \input ->
    input
    |> Str.split "\n"
    |> List.dropIf (\s -> s == "")
    |> List.map (\s ->
        Str.split s " "
        |> List.mapTry Str.toI64
        |> crashResult "not all numbers"
    )

part1 = \input ->
    input
    |> parse
    # COMPUTE
    |> List.map next
    |> List.sum
    # REPORT
    |> Inspect.toStr
    |> Stdout.line

next : List I64 -> I64
next = \ns ->
    diffs : List (List I64)
    diffs =
        findDiffs [] ns

    diffs
    |> List.walkBackwards 0 (\bottom,top ->
        when (top) is
            [.., topLeft] ->
                bottom + topLeft

            _ ->
                crash "next: wut"
    )

findDiffs : List (List I64), List I64 -> List (List I64)
findDiffs = \acc,current ->
    if List.all current (\n -> n == 0) then
        acc

    else
        newCurrent = current |> mapPairs (\a,b -> b - a)
        newAcc = acc |> List.append current
        findDiffs newAcc newCurrent

expect findDiffs [] [0,3,6,9,12,15] == [[0,3,6,9,12,15],[3,3,3,3,3]]

mapPairs : List a, (a,a -> b) -> List b
mapPairs = \xs,fn ->
    List.map2 
    xs
    (xs |> List.dropFirst 1)
    fn

expect mapPairs [1,2,3] Pair == [Pair 1 2, Pair 2 3]

crashResult : Result a *, Str -> a
crashResult = \result,label ->
    when result is
        Ok a -> a
        Err _ -> crash label

### PART 2

part2 = \input ->
    input
    |> parse
    # COMPUTE
    |> List.map previous
    |> List.sum
    # REPORT
    |> Inspect.toStr
    |> Stdout.line

previous : List I64 -> I64
previous = \ns ->
    diffs : List (List I64)
    diffs =
        findDiffs [] ns

    diffs
    |> List.walkBackwards 0 (\bottom,top ->
        when (top) is
            [topRight, ..] ->
                topRight - bottom

            _ ->
                crash "previous: wut"
    )
