app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
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
