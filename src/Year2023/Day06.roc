app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
    ]
    provides [main] to pf


main = part1 realInput1
#main = part2 realInput2
#main = part1 testInput1
#main = part2 testInput2

part1 = \input ->
    input
    |> List.map process1
    |> List.product
    |> Inspect.toStr
    |> Stdout.line

process1 = \(ms,record) ->
    List.range { start: At 1, end: Before ms }
    |> List.countIf (\n -> (ms - n) * n > record)

part2 = \_ ->
    DoneByHand

# Part 2:
# Test input:
# -----------------

# Limits
# min n: (71530 - n) * n > 940200
# max n: (71530 - n) * n > 940200

# The inequation:
# 71530n - n^2 > 940200
# -n^2 + 71530n - 940200 > 0
# https://www.wolframalpha.com/input?i=-n%5E2+%2B+71530n+-+940200+%3E+0

# The solutions:
# 14..71516
# Count: 71516-14+1 = 71503

# Now for the real input:
# ---------------------------
# inequation: 
# -n^2 + 44826981n - 202107611381458 > 0

# https://www.wolframalpha.com/input?i=-n%5E2+%2B+44826981n+-+202107611381458+%3E+0+over+integers

# solutions:
# 5085567<=n<=39741414
# Count: 34655848

testInput1 =
    [ (7,9)
    , (15,40)
    , (30,200)
    ]

realInput1 =
    [ (44,202)
    , (82,1076)
    , (69,1138)
    , (81,1458)
    ]

testInput2 =
    (71530,940200)

realInput2 =
    (44826981,202107611381458)
