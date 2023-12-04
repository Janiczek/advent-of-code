app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "../../input202304.txt" as realInput : Str
    ]
    provides [main] to pf

#main = part1 realInput
main = part2 realInput
#main = part1 testInput
#main = part2 testInput

parse : Str -> List {id: Nat, winning: Set Nat, your: Set Nat}
parse = \input ->
    input
    |> Str.split "\n"
    |> List.dropIf (\s -> s == "")
    |> List.map parseRow

dropFirst : Str,Nat -> Str
dropFirst = \str,n ->
    str
    |> Str.graphemes
    |> List.dropFirst n
    |> List.walk "" Str.concat

parseRow : Str -> Card
parseRow = \line ->
    when line |> Str.split ": " is
        [cardStr, numsStr] -> 
            id = 
                cardStr
                |> dropFirst 5
                |> Str.trimStart
                |> Str.toNat
                |> Result.withDefault 999999999

            when numsStr |> Str.split " | " is
                [winningStr, yourStr] ->
                    {
                        id: id,
                        winning: parseList winningStr,
                        your: parseList yourStr,
                    }
                _ -> crash "parseRow didn't see ' | '"
        _ -> crash "parseRow didn't see ': '"

Card :
    {
        id: Nat, 
        winning: Set Nat, 
        your: Set Nat,
    }

parseList : Str -> Set Nat
parseList = \line ->
   line
   |> Str.split " "
   |> List.dropIf (\str -> str == "")
   |> List.mapTry Str.toNat
   |> Result.withDefault []
   |> Set.fromList

score : Card -> Nat
score = \card ->
    Set.intersection card.winning card.your
    |> Set.len
    |> (\p -> 
        if p == 0 then
            0
        else 
            Num.powInt 2 (Num.subSaturated p 1)
    )
    
go2 : List Card, Dict Nat Nat -> Nat
go2 = \todos,inventory ->
    when todos is
        [todo, .. as rest] ->
            matches = Set.len (Set.intersection todo.winning todo.your)
            copies = Dict.get inventory todo.id |> Result.withDefault 88888888
            newInventory = 
                if matches == 0 then
                    inventory
                else
                    List.range {start: At (todo.id + 1), end: At (todo.id + matches)}
                    |> List.walk inventory (\accInv,id -> 
                        wonCurrentCopies = Dict.get accInv id |> Result.withDefault 7777777777
                        Dict.insert accInv id (wonCurrentCopies + copies)
                    )

            go2 rest newInventory
            
        [] ->
            inventory
            |> Dict.values
            |> List.sum



part1 = \input ->
    input
    |> parse
    |> List.map score
    |> List.sum
    |> Num.toStr
    |> Stdout.line

part2 = \input ->
    cards = parse input
    initInventory = 
        List.range { start: At 1, end: At (List.len cards) }
        |> List.map (\id -> (id, 1))
        |> Dict.fromList

    cards
    |> go2 initInventory
    |> Num.toStr
    |> Stdout.line

testInput : Str
testInput =
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
