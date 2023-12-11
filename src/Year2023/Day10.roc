app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "../../input202310.txt" as realInput : Str
    ]
    provides [main] to pf

crashResult : Result a *, Str -> a
crashResult = \result,label ->
    when result is
        Ok a -> a
        Err _ -> crash label

testInput =
    """
    ..F7.
    .FJ|.
    SJ.L7
    |F--J
    LJ...
    """

main = part1 realInput
#main = part2 realInput
#main = part1 testInput
#main = part2 testInput2

part1 = \input ->
    grid = parse input

    sPosition = 
        grid
        |> Dict.toList
        |> List.findFirst (\(_,pipe) -> pipe == Start)
        |> crashResult "S not found"
        |> (\(xy,_) -> xy)

    findMaxDistance grid 0 [sPosition]
    |> Inspect.toStr
    |> Stdout.line

gridToStr : Grid -> Str
gridToStr = \grid ->
    maxX = grid |> Dict.toList |> List.map (\((x,_),_) -> x) |> List.max |> Result.withDefault 0
    maxY = grid |> Dict.toList |> List.map (\((_,y),_) -> y) |> List.max |> Result.withDefault 0
    List.range {start: At 0, end: At maxY}
    |> List.map (\y -> 
        List.range {start: At 0, end: At maxX}
        |> List.map (\x -> 
            when Dict.get grid (x,y) is
                Ok Vertical -> "│"
                Ok Horizontal -> "─"
                Ok TopLeft -> "┌"
                Ok TopRight -> "┐"
                Ok BottomLeft -> "└"
                Ok BottomRight -> "┘"
                Ok Start -> "S"
                Err _ -> "."
        )
        |> Str.joinWith ""
    )
    |> Str.joinWith "\n"

findMaxDistance : Grid,Nat,List XY -> Nat
findMaxDistance = \grid,step,current ->
    #dbg "----------"
    #dbg step
    #dbg (Str.concat "\n" (gridToStr grid))
    #dbg ("curr",current)

    next = 
        current 
        |> List.joinMap (\xy -> findNeighbours grid xy)

    #dbg ("next",next)

    if List.isEmpty next then
        step
    else
        newGrid = current |> List.walk grid Dict.remove
        findMaxDistance newGrid (step + 1) next

findNeighbours : Grid,XY -> List XY
findNeighbours = \grid,xy ->
    tt = t xy
    bb = b xy
    ll = l xy
    rr = r xy
    when Dict.get grid xy is
        Err _ -> crash "find neighbours - no self"
        Ok Start ->
            [ if hasBEdge grid tt then Ok tt else Err NotFound
            , if hasTEdge grid bb then Ok bb else Err NotFound
            , if hasLEdge grid rr then Ok rr else Err NotFound
            , if hasREdge grid ll then Ok ll else Err NotFound
            ]
            |> List.keepOks (\x -> x)
        Ok Vertical ->
            [ if hasBEdge grid tt then Ok tt else Err NotFound
            , if hasTEdge grid bb then Ok bb else Err NotFound
            ]
            |> List.keepOks (\x -> x)
        Ok Horizontal ->
            [ if hasLEdge grid rr then Ok rr else Err NotFound
            , if hasREdge grid ll then Ok ll else Err NotFound
            ]
            |> List.keepOks (\x -> x)
        Ok TopLeft ->
            [ if hasTEdge grid bb then Ok bb else Err NotFound
            , if hasLEdge grid rr then Ok rr else Err NotFound
            ]
            |> List.keepOks (\x -> x)
        Ok TopRight ->
            [ if hasTEdge grid bb then Ok bb else Err NotFound
            , if hasREdge grid ll then Ok ll else Err NotFound
            ]
            |> List.keepOks (\x -> x)
        Ok BottomLeft ->
            [ if hasBEdge grid tt then Ok tt else Err NotFound
            , if hasLEdge grid rr then Ok rr else Err NotFound
            ]
            |> List.keepOks (\x -> x)
        Ok BottomRight ->
            [ if hasBEdge grid tt then Ok tt else Err NotFound
            , if hasREdge grid ll then Ok ll else Err NotFound
            ]
            |> List.keepOks (\x -> x)

hasBEdge : Grid,XY -> Bool
hasBEdge = \grid,xy ->
    res = Dict.get grid xy
    res == Ok Vertical || res == Ok TopLeft || res == Ok TopRight

hasTEdge : Grid,XY -> Bool
hasTEdge = \grid,xy ->
    res = Dict.get grid xy
    res == Ok Vertical || res == Ok BottomLeft || res == Ok BottomRight

hasLEdge : Grid,XY -> Bool
hasLEdge = \grid,xy ->
    res = Dict.get grid xy
    res == Ok Horizontal || res == Ok TopRight || res == Ok BottomRight

hasREdge : Grid,XY -> Bool
hasREdge = \grid,xy ->
    res = Dict.get grid xy
    res == Ok Horizontal || res == Ok TopLeft || res == Ok BottomLeft

Pipe: [
    Vertical, # |
    Horizontal, # -
    TopLeft, # F
    TopRight, # 7
    BottomLeft, # L
    BottomRight, # J
    Start, # S
]
XY: (I64,I64)
Grid: Dict XY Pipe

t = \(x,y) -> (x,y-1)
b = \(x,y) -> (x,y+1)
l = \(x,y) -> (x-1,y)
r = \(x,y) -> (x+1,y)

parse : Str -> Grid
parse = \input -> 
    input
    |> Str.split "\n"
    |> List.mapWithIndex (\row,y ->
        row
        |> Str.graphemes
        |> List.mapWithIndex (\char,x ->
            xy = (Num.intCast x, Num.intCast y)
            when char is
                "|" -> Ok (xy,Vertical)
                "-" -> Ok (xy,Horizontal)
                "J" -> Ok (xy,BottomRight)
                "L" -> Ok (xy,BottomLeft)
                "F" -> Ok (xy,TopLeft)
                "7" -> Ok (xy,TopRight)
                "S" -> Ok (xy,Start)
                "." -> Err DoNotCareAboutGround
                _ -> crash "parse: unknown char"
        )
        |> List.keepOks (\x -> x)
    )
    |> List.join
    |> Dict.fromList
