app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Task,
        pf.Stdout,
        "../../input202303.txt" as realInput : Str
    ]
    provides [main] to pf

main = 
    #{} <- part1 "P1 test: " testInput |> Task.await
    #{} <- part1 "P1 REAL: " realInput |> Task.await
    {} <- part2 "P2 test: " testInput |> Task.await
    {} <- part2 "P2 REAL: " realInput |> Task.await
    Task.ok {}

digits : Set Str
digits =
    Set.fromList [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]

nonSymbols : Set Str
nonSymbols =
    Set.insert digits "."

isSymbol : Str -> Bool
isSymbol = \str ->
    Bool.not (Set.contains nonSymbols str)

isDigit : Str -> Bool
isDigit = \str ->
    Set.contains digits str

allPositions : Str -> Set (Nat,Nat,Str)
allPositions = \str ->
    str
    |> Str.split "\n"
    |> List.keepIf (\s -> Str.countGraphemes s > 0)
    |> List.mapWithIndex (\s,y -> 
        s 
        |> Str.graphemes 
        |> List.mapWithIndex (\char,x -> (x,y,char))
    )
    |> List.join
    |> Set.fromList

expect allPositions "ab\ncd" == Set.fromList [(0,0,"a"),(1,0,"b"),(0,1,"c"),(1,1,"d")]

symbolPositions : Set (Nat,Nat,Str) -> Set (Nat,Nat)
symbolPositions = \all ->
    all
    # Would love Set.keepIf ...
    |> Set.toList
    |> List.keepIf (\(_,_,c) -> isSymbol c)
    |> Set.fromList
    |> Set.map (\(x,y,_) -> (x,y))

gearPositions : Set (Nat,Nat,Str) -> Set (Nat,Nat)
gearPositions = \all ->
    all
    # Would love Set.keepIf ...
    |> Set.toList
    |> List.keepIf (\(_,_,c) -> c == "*")
    |> Set.fromList
    |> Set.map (\(x,y,_) -> (x,y))

# Causes infinite loop in `roc check`:
# expect symbolPositions (Set.fromList [(0,0,"."),(1,0,"="),(0,1,"c"),(1,1,"d")]) == Set.fromList [(1,0,"=")]

# neighbours of a position = set of positions
# if any neighbour of the number is a symbol, use it, otherwise skip it

numbers : Str -> List (Nat, Set (Nat,Nat))
numbers = \str ->
    str
    |> Str.graphemes
    |> List.walk
        { 
            acc: [],
            x: 0,
            y: 0,
            numAcc: "",
            posAcc: Set.empty {},
        }
        (\state,char ->
            isInMiddleOfNumber = state.numAcc != ""
            isNewline = char == "\n"
            # can't use isDigit', can't use isDigit_, shadowing disallowed... what do people use?
            isDigitX = isDigit char
            newAcc = 
                if isDigitX then
                    state.acc
                else
                    (if isInMiddleOfNumber then
                         # finish
                         Str.toNat state.numAcc
                         |> Result.map (\n -> List.append state.acc (n, state.posAcc))
                         |> Result.withDefault state.acc
                     else
                         # ignore
                         state.acc
                    )
            newX = if isNewline then 0 else state.x + 1
            newY = if isNewline then state.y + 1 else state.y
            newNumAcc =
                if isDigitX then
                    Str.concat state.numAcc char
                else
                    ""
            newPosAcc =
                if isDigitX then 
                    Set.insert state.posAcc (state.x, state.y)
                else 
                    Set.empty {}
            { state & 
                acc: newAcc,
                x: newX,
                y: newY,
                numAcc: newNumAcc,
                posAcc: newPosAcc,
            }
        )
    |> .acc

neighbours : (Nat,Nat) -> List (Nat,Nat)
neighbours = \(x,y) ->
    [ (Num.subWrap x 1, Num.subWrap y 1), (x,Num.subWrap y 1), (x+1,Num.subWrap y 1)
    , (Num.subWrap x 1, y),                                    (x+1,y)
    , (Num.subWrap x 1, y+1),             (x,y+1),             (x+1,y+1)
    ]

anyAdjacentToSymbol : Set (Nat,Nat), Set (Nat,Nat) -> Bool
anyAdjacentToSymbol = \symbols,numberDigits ->
        numberDigits
        |> Set.toList
        |> List.any (\pos -> 
            neighbours pos
            |> List.any (\neighbour -> Set.contains symbols neighbour)
        )

part1 = \label,input ->
    all = allPositions input
    symbols = symbolPositions all

    numbers input
    |> List.keepIf (\(_,positions) -> anyAdjacentToSymbol symbols positions)
    |> List.map (\(num,_) -> num)
    |> List.sum
    |> Num.toStr
    |> (\s -> Str.concat label s)
    |> Stdout.line

part2 = \label,input -> 
    all = allPositions input
    nums = numbers input

    # Would love List.filterMap
    gearPositions all
    |> Set.toList
    |> filterMap (\gearPos -> 
        ns = neighbours gearPos
        neighbouringNumbers =
            ns
            |> filterMap (\neighbour ->
                when nums |> List.findFirst (\(_,nPositions) -> Set.contains nPositions neighbour) is
                    Ok (n,_) -> 
                        Keep n
                    Err NotFound ->
                        Drop

            )
            |> Set.fromList
            |> Set.toList

        if List.len neighbouringNumbers == 2 then
            Keep (List.product neighbouringNumbers)
        else
            Drop
    )
    |> List.sum
    |> Num.toStr
    |> (\s -> Str.concat label s)
    |> Stdout.line

filterMap : List a, (a -> [Keep b, Drop]) -> List b
filterMap = \xs,fn ->
    xs
    |> List.map (\x ->
        when fn x is
            Keep b -> [b]
            Drop -> []
    )
    |> List.join


testInput = 
  "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."

