app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Stdout,
        pf.Task,
        "../../input202302.txt" as realInput : Str
    ]
    provides [main] to pf


main = part1 realInput
#main = part2 realInput
#main = part1 testInput
#main = part2 testInput

parseRow = \rowStr ->
    when rowStr |> Str.split ": " is
        [idStr, logStr] ->
            id <-
                idStr
                |> # Missing Str.left or Str.dropLeft and so on
                   Str.graphemes
                |> List.dropFirst 5
                |> List.walk "" Str.concat
                |> Str.toNat
                |> Result.try

            games <- 
                logStr
                |> Str.split "; "
                |> List.mapTry parseDraw
                |> Result.try

            Ok {
                id: id,
                games: games,
            }

        _ -> crash "parse error"

parseDraw = \drawStr ->
    drawStr 
    |> Str.split ", " 
    |> List.mapTry \colorDrawStr -> 
        when colorDrawStr |> Str.split " " is
            [nStr, color] -> 
                n <- Str.toNat nStr |> Result.try
                Ok (color,n)
            _ -> Err NoColor


isValidPart1Row = \row ->
    row.games
    |> List.all \game -> 
        game
        |> List.all \(color,n) -> 
            if color == "red" then n <= 12 else
            if color == "green" then n <= 13 else
            if color == "blue" then n <= 14 else 
            Bool.true

part1 = \input ->
    rows <- 
        input
            |> Str.split "\n"
            |> List.keepIf (\s -> s |> Str.isEmpty |> Bool.not)
            |> List.mapTry parseRow
            |> Result.mapErr \err ->
                when err is
                    InvalidNumStr -> -1
                    NoColor -> -2
            |> Task.fromResult
            |> Task.await

    rows
    |> List.keepIf isValidPart1Row
    |> List.map .id
    |> List.sum
    |> Num.toStr
    |> Stdout.line
