app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "../../input202305.txt" as realInput : Str
    ]
    provides [main] to pf


#main = part1 realInput
main = part2 realInput
#main = part1 testInput
#main = part2 testInput

Range :
    {
        destRangeStart: Nat,
        sourceRangeStart: Nat,
        length: Nat,
    }

Input : 
    {
        seeds: List Nat,
        maps: List (List Range),
    }

parse : Str -> Input
parse = \str ->
    sections =
        str
        |> Str.split "\n\n"
        |> List.dropIf (\s -> s == "")
        |> List.map Str.trim

    when sections is
        [] -> crash "parse: empty \\n\\n split"
        [seedsStr, .. as mapsStrs] ->
            seeds = 
                seedsStr
                |> Str.split " "
                |> List.dropFirst 1
                |> List.mapTry Str.toNat
                |> crashResult "not all seeds are numbers"

            maps =
                mapsStrs
                |> List.map (\mapStr ->
                    mapStr
                    |> Str.split "\n"
                    |> List.dropIf (\s -> s == "")
                    |> List.map Str.trim
                    |> List.dropFirst 1
                    |> List.map (\mapLine ->
                        nums =
                            mapLine
                            |> Str.split " "
                            |> List.mapTry Str.toNat
                            |> crashResult "not all map contents are numbers"

                        when nums is
                            [dst,src,len] ->
                                {
                                    destRangeStart: dst,
                                    sourceRangeStart: src,
                                    length: len,
                                }
                            _ ->
                                crash "Unexpected nums in map"
                    )
                )

            {
                seeds: seeds,
                maps: maps,
            }

crashResult : Result a *, Str -> a
crashResult = \result,label ->
    when result is
        Ok a -> a
        Err _ -> crash label


part1 = \input ->
    in = parse input

    in.seeds
    |> List.map (\seed -> follow1 in.maps seed)
    |> List.min
    |> Inspect.toStr
    |> Stdout.line

follow1 : List (List Range),Nat -> Nat
follow1 = \maps,seed ->
    maps
    |> List.walk seed (\acc,map ->
        map
        |> List.walk (NotFound acc) (\acc2,range ->
            when acc2 is
                NotFound theSeed ->
                    if theSeed >= range.sourceRangeStart
                    && theSeed < range.sourceRangeStart + range.length then
                        Found (range.destRangeStart + (theSeed - range.sourceRangeStart))
                    else
                        NotFound theSeed

                # "Short circuit"
                Found theSeed ->
                    Found theSeed
        )
        |> unwrapFound
    )

unwrapFound : [ Found a, NotFound a ]  -> a
unwrapFound = \x ->
    when x is
        Found a -> a
        NotFound a -> a

part2 = \input ->
    in = parse input
    seedGroups = groupBy2 in.seeds
    seedGroupsLen = List.len seedGroups

    seedGroups
    |> List.map (\group -> walkGroup in.maps 9999999999 group)
    |> List.min
    |> Inspect.toStr
    |> Stdout.line

walkGroup : List (List Range),Nat,(Nat,Nat) -> Nat
walkGroup = \maps,minSoFar,(start,len) ->
    if len <= 0 then
        minSoFar
    else
        x = 
            if start % 1000000 == 0 then
                dbg start
                start
            else
                start

        result = follow1 maps start
        newMin = Num.min result minSoFar
        walkGroup maps newMin (start + 1, len - 1)

groupBy2 : List a -> List (a,a)
groupBy2 = \list ->
    groupBy2Help list []

groupBy2Help : List a, List (a,a) -> List (a,a)
groupBy2Help = \list,acc ->
    when list is
        [a,b, .. as rest] ->
            groupBy2Help rest (acc |> List.append (a,b))
        _ ->
            acc


testInput =
    "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4"
