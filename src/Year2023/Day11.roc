app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "../../input202311.txt" as realInput : Str
    ]
    provides [main] to pf


#main = part1 realInput
main = part2 realInput
#main = part1 testInput
#main = part2 testInput


testInput =
    """
    ...#......
    .......#..
    #.........
    ..........
    ......#...
    .#........
    .........#
    ..........
    .......#..
    #...#.....
    """

part1 = \input ->
    input
    |> parse
    |> expandRows
    |> expandColumns
    |> toGalaxies
    |> toAllPairs
    |> List.map manhattanDistance
    |> List.sum
    |> Inspect.toStr
    |> Stdout.line

part2 = \input ->
    parsed = parse input
    emptyRows = findEmpty parsed
    emptyColumns = findEmpty (transpose parsed)

    parsed
    |> toGalaxies
    |> expandSparse emptyRows emptyColumns 1000000
    |> toAllPairs
    |> List.map manhattanDistance
    |> List.sum
    |> Inspect.toStr
    |> Stdout.line

expandSparse : List XY,List I64,List I64,I64 -> List XY
expandSparse = \galaxies,emptyRows,emptyColumns,mult ->
    galaxies
    |> List.map (\(x,y) -> 
        dx = emptyColumns |> List.countIf (\c -> c < x) |> Num.intCast |> Num.mul (mult - 1)
        dy = emptyRows    |> List.countIf (\r -> r < y) |> Num.intCast |> Num.mul (mult - 1)
        (x+dx,y+dy)
    )

expect expandSparse [(0,0),(2,3)] [1,2] [1] 3 == [(0,0),(4,7)]

parse : Str -> List (List Str)
parse = \input ->
    input
    |> Str.split "\n"
    |> List.dropIf (\s -> s == "")
    |> List.map Str.graphemes

XY: (I64,I64)

findEmpty : List (List Str) -> List I64
findEmpty = \rows ->
    rows 
    |> List.mapWithIndex (\row,y -> 
        if Set.fromList row == Set.single "." then
            Ok (Num.intCast y)
        else
            Err NotEmpty
    )
    |> List.keepOks (\x -> x)



expandRows : List (List Str) -> List (List Str)
expandRows = \rows ->
    rows
    |> List.joinMap (\row ->
        if Set.fromList row == Set.single "." then
            [row,row]
        else
            [row] 
    )

expandColumns : List (List Str) -> List (List Str)
expandColumns = \rows ->
    rows
    |> transpose
    |> expandRows
    |> transpose

transpose : List (List a) -> List (List a)
transpose = \listOfLists ->
    listOfLists
    |> List.walk 
        ([] |> List.repeat (rowsLength listOfLists))
        (\acc,list ->
            List.map2 
                acc
                list
                List.append
        )

expect transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]

rowsLength : List (List a) -> Nat
rowsLength = \lists ->
    when lists is
        [x,..] -> List.len x
        _ -> 0

toGalaxies : List (List Str) -> List XY
toGalaxies = \input ->
    input
    |> List.mapWithIndex (\row,y ->
        row |> List.mapWithIndex (\cell,x ->
            if cell == "#" then
                Ok (Num.intCast x, Num.intCast y)
            else
                Err NotAGalaxy
        )
    )
    |> List.join
    |> List.keepOks (\x -> x)

toAllPairs : List a -> List (a,a)
toAllPairs = \xs ->
    when xs is
        [] -> []
        [x, .. as rest] ->
            List.concat
                (rest |> List.map (\y -> (x,y)))
                (toAllPairs rest)

expect toAllPairs [1,2,3,4] == [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

manhattanDistance : (XY,XY) -> I64
manhattanDistance = \((x1,y1),(x2,y2)) ->
    Num.abs (x2 - x1) + Num.abs (y2 - y1)
