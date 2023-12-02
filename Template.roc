app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Stdout,
        "../../inputXXX.txt" as realInput : Str
    ]
    provides [main] to pf


main = part1 realInput
#main = part2 realInput
#main = part1 testInput
#main = part2 testInput

part1 = \input ->
    input
    |> Stdout.line
