module AoC.Day20

open FSharpPlus

let exampleInput = "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###..."

let realInput = "Tile 2411:
.#.##..#.#
.#.......#
.##...#.#.
......#...
#.#.......
##.....#.#
#..#...#..
##........
##....#...
##..#.##.#

Tile 1997:
#.#......#
.#......##
#.##..#..#
...#..#.#.
.#.#...#..
#.........
..#..#....
#.#..#.##.
#..##.....
#...#.#.#.

Tile 1427:
#...#..#..
..##.....#
..#...####
#..#.#...#
..#.#.#..#
..####....
#.#.###.#.
......#...
#.........
#.#.##...#

Tile 2161:
###.####.#
#....###.#
#.#......#
..#....#..
...#.....#
.#..#...##
#.#.....##
#..#......
.....#.#..
...###.#.#

Tile 1321:
###...#.#.
.#...#..##
.#.......#
..........
#.#.......
....#.....
######....
#.....#...
##.......#
#.###..###

Tile 1181:
.#..#..###
###...#.##
#.........
....###..#
#.#...#..#
#....###..
##..##...#
#....##.##
#.......#.
###.##...#

Tile 2749:
#####..##.
##........
......#.#.
#..#...#.#
.....##.#.
.....#...#
#.........
#........#
....#..#.#
.#..#...#.

Tile 3911:
##...#####
#...#..#.#
.##...#..#
#...#.....
#..#.#...#
..#......#
#.#.......
....##..#.
..#.#..##.
####.#..#.

Tile 3257:
#..#.##...
.#.....#.#
##.#.....#
##.#.#....
#..##..#..
........##
#.##...#.#
#.#..#...#
........##
.##.####.#

Tile 2237:
.###.#####
....#....#
.......###
#........#
..#.......
.#.#......
#...##...#
#.....#.#.
#...#.....
####.#####

Tile 2389:
#.#.###...
#.#.#...##
......#...
.####....#
..#..#..#.
#.......##
..###....#
#........#
#....##...
..#.#..##.

Tile 3209:
..##.#####
.........#
##..#.##..
...#.#...#
....#.#..#
#...#.....
#.#.#.....
#...#....#
#.#......#
...#..####

Tile 2579:
#...#.##..
....##.###
#......###
.......#..
##...#....
#.#......#
.##.#....#
.#.......#
#...#.....
##..#.##..

Tile 2087:
.###.##.#.
.##...##.#
..####.#.#
...#......
...#......
......#..#
#.##......
.#.#.#.#..
....##..##
...##.#..#

Tile 3517:
###.#...##
#.........
#.....#...
###..#.#..
##......#.
..#....#.#
#.....#...
..#...#.##
##...#.#..
##..##.###

Tile 3347:
##.#..#...
.........#
..........
.#.#.....#
..........
....#..#.#
#........#
##.#...#.#
....#..#..
##.#.#....

Tile 2711:
.##.#.#...
...#.#....
..........
#......###
..##......
#.........
#..#..##..
.##...##..
#......#..
#..#.##.#.

Tile 3877:
###.#..#.#
#...##...#
..#.#..#.#
##......##
.#........
##..#....#
..###..#.#
#.#.......
#.#......#
...###.###

Tile 1721:
###...##.#
......#..#
.##.##....
....##.#..
##..#..#..
###..#..#.
####..##.#
#.#.#....#
.......#.#
#..##....#

Tile 3457:
.#.#..##.#
..##.#.#..
##...#...#
...#......
..#....##.
#...#.#..#
..#.......
...#......
........#.
.#.#.##..#

Tile 1231:
.#.......#
#........#
#..#..#...
..#......#
.....#....
#.........
#...#..#.#
.....#..#.
.#.#....#.
###...##..

Tile 1901:
.###.#.#.#
...##.....
#......#.#
...##.....
....#.....
#.........
..#......#
##.....#..
#.........
#..##..###

Tile 3329:
#..#..#.#.
....#.....
###.....##
.........#
##..#.....
#.......##
#..#.....#
#..#.#..#.
...#......
...#...##.

Tile 1291:
..#....#.#
#.#...##.#
#..#....##
#..###...#
#...#.#...
####......
#.....##.#
#.#..#....
##....#..#
#.###.#...

Tile 1481:
#..#...##.
....#..#..
...##.....
#..#..##..
###.#..##.
##.#.##...
#..#.....#
#......#.#
#.#....#..
.#..#..#.#

Tile 2063:
.###..#..#
...#......
#..#.....#
#..#..#...
.....#.###
...#......
#.#.....#.
#........#
.#...#...#
##.#....##

Tile 2297:
.#..#..#.#
#..#......
#......#.#
#.#..###.#
..........
##..#.#.##
#......#..
#....#.#.#
#........#
.#..###...

Tile 2731:
..#.......
......#...
#..#...#..
..........
#..#...#..
#.#..#..##
##..#..##.
..........
#.##..#...
##....##..

Tile 1453:
....#.##.#
#..#..##.#
##.#.#....
#..##.#...
#......#..
#......###
...##...#.
#.##......
##........
#######.##

Tile 1117:
.###.#...#
.#.#.....#
..#.#...##
#.......#.
#.....#..#
#.###....#
##.#......
#.####..##
#..#..#.#.
##...#####

Tile 1823:
#.....#...
#......##.
.....#.#..
......###.
..#..###.#
....#.##.#
.##..#.#.#
#.#.......
##........
.#...##..#

Tile 3163:
..#####.##
...#......
#..###...#
..##..#...
.#.......#
..#....#.#
###.#.....
#..#..#...
..##.#....
.##.#.#..#

Tile 2083:
#..######.
....##.#.#
.....###.#
#...#..#..
#....#...#
...#...#.#
#..####...
#....#....
#..#....#.
#....#.#..

Tile 2659:
#.###.#...
...#......
#..#....##
..###.##..
##.......#
#.......#.
#....#...#
#..#.....#
..#.#.....
..#..##.#.

Tile 2113:
.##.######
.......###
#....#....
....#..#..
#.##......
####..##..
.##..####.
...#....##
.#.....#..
#.#.##..##

Tile 3943:
###..#....
.#.....#..
#.........
##....##..
#.#....#.#
#.#.#.....
#....#..#.
.##..##..#
....##....
.#.#.#####

Tile 3719:
#...#.#.#.
.........#
#.#.......
......#.#.
.#...#.#.#
.#....####
##........
.##..#.#.#
#.........
#.#.#....#

Tile 1409:
####.#####
#....#....
##.#....#.
#..#......
#....#....
#.#...#...
##.....#..
#.....#..#
......#...
..####...#

Tile 3547:
.#########
.#.##...#.
...#..#.#.
#.##...###
#......#..
##...#....
##.#.#.###
##........
.....#...#
###.##.##.

Tile 1123:
...##....#
.........#
#...#...##
..........
#.........
#......#.#
#..#.#.#.#
......#..#
#.#...#.##
####.#....

Tile 2203:
.####.#..#
##........
##.......#
#....#....
.#......##
.#..#..#.#
......#..#
##.###...#
#....#....
.##..##.##

Tile 3511:
#.#..#.#.#
#......###
#..#....#.
#.####....
##...#...#
#.##...##.
##.##..##.
.....#...#
#...##...#
.#.#..##.#

Tile 3499:
..#..#.###
#.........
..##..#...
...#.#.#.#
...#.#.#..
#.....##.#
...#......
..#...###.
##..###..#
#.#....###

Tile 1039:
#.#....##.
#..###.#.#
###.....#.
##..#....#
.#....##.#
#..#######
.##.#....#
#..#......
##.#...##.
#..#..##.#

Tile 3323:
#..#.#.##.
#......#..
.........#
.........#
.#....##.#
#.....#..#
#....##...
#...##...#
.###...#.#
#...###.##

Tile 1033:
###...##.#
#..#..#.##
.........#
.#.#.#..#.
#.........
#......##.
#...#.##.#
..........
###.......
.##.#.....

Tile 2269:
....#.##..
#........#
.#..#.#..#
#......#.#
#...#.###.
#..#.....#
....#....#
........##
#.#...#...
.#...#.#.#

Tile 1279:
...##.###.
....#...#.
#..#......
#......##.
.#.#....##
.#...##..#
..#..#...#
#......#..
#..#....#.
.#.#...#.#

Tile 2549:
..#..###..
..........
........##
....#....#
.........#
#...#..#.#
....##..#.
.#........
..##..#...
##.##.####

Tile 2377:
..#.######
..##..#..#
..#.....#.
#.#.#.#...
#....#....
..........
#..##....#
#...####..
.......#.#
#.#.#..##.

Tile 3169:
.######.##
#..#..####
#.....#..#
#.##..##.#
....#...##
..##......
#..#...###
.....##...
#......###
#.#....#..

Tile 1979:
###.#.##..
#..#...#..
#..#..#.#.
....##....
#.#.......
#..#.....#
....#.....
.#..#..#.#
#.#.###..#
#.####.#.#

Tile 2131:
..##..#.##
##...#....
#...#.#..#
.....#...#
.#.#.#..#.
##......#.
......#...
#..##.....
.###...###
#...######

Tile 2531:
.#.######.
....#..#.#
...###...#
.........#
#....#.#..
.....#...#
#...#....#
....#.....
..#.......
.....#.##.

Tile 3167:
####.###..
#..#.....#
#.......##
##..#..#.#
#.........
#.....#.#.
##.......#
...#.#....
.........#
######..##

Tile 3191:
#....#####
#...#.....
..##...#..
#.......#.
#..#.###.#
#####...##
#.#...###.
#.#..#.##.
#.........
#.##.##.#.

Tile 3767:
#..#..####
#......#..
#.#.##....
.....#..#.
.#...#.#..
..#.#..###
..##......
.###.##..#
.##....#.#
.##.##..##

Tile 2633:
##...##.##
#....#..##
...##.#...
#.#.#.....
.......###
.#...#....
#....#.###
.#.....#.#
##....#...
....#..##.

Tile 2677:
...#.##.#.
......#...
.....#....
#......#..
#...#.###.
..#....#.#
.........#
#..#...#.#
#.#....###
#.#..#....

Tile 3989:
.###.###.#
#.#..#..##
......#.#.
...#......
..####...#
....###.#.
#.###.#..#
....##....
#.........
.#.##.####

Tile 2897:
####...#.#
..........
#.#.......
#...#.....
......#...
##.###.#..
.##..##..#
...##.##..
...##...#.
.#.######.

Tile 3803:
#.#..#.##.
###....#..
#...#..#.#
.......###
#.....#..#
##.....#..
##..##..##
.#....##.#
.###......
#..#.#####

Tile 1187:
.#.#####..
.#.#......
#.#.####.#
#...#.....
..#.......
.#.....#..
#...#.#..#
#......###
#.##..#..#
#.##.##.##

Tile 1801:
#.##...#..
.##......#
#..####..#
#.##...#.#
#......#.#
.....#.#..
..###...#.
..#.#.....
#...#.#...
#....####.

Tile 2729:
.......#..
#.#...#..#
..#.#..#..
.....#.#..
#.........
#.#..#....
.#.......#
#.#....#.#
#.#...#.##
##.#.###.#

Tile 2969:
.......#.#
##..#....#
#.....#.##
##.....#.#
#..#......
.#.#.#....
#....#.#.#
##......##
#......##.
.##.##....

Tile 3079:
...##.####
.....#...#
..........
..#.#.....
.........#
.#...##.##
.###.....#
..#.#..##.
#...##...#
.##...##..

Tile 3407:
...#.#.#..
.#...##.##
##....#..#
.#....#.#.
.##.#....#
##......##
#...#...##
#..#.....#
#.....#...
##....##.#

Tile 3389:
####.#..#.
#..##.....
##...##.#.
#......#..
.....#..##
#...##..##
#...##...#
...##..#..
....##..##
#.#...#...

Tile 1663:
#...##.##.
..#..#.#.#
#.....#.#.
#.........
..##.##..#
.....#..#.
#....#....
#.##......
.##.#...##
.....#...#

Tile 1787:
#..##....#
......#.#.
#......##.
#.#......#
#....#...#
#.#.......
.....#...#
#...#..#..
.#......##
..###.#.##

Tile 2917:
##.#.#####
#..#..#.##
..##..#..#
#..#.##.##
#.#..#....
.#.#.##..#
.####.....
#..#...#.#
#...#....#
.#.#.##..#

Tile 3067:
##.#.#...#
.###..#..#
.##.#.#...
#.#.....##
..#.......
#...#...#.
##..#....#
...##..#.#
#.......#.
.###...#.#

Tile 2963:
.#..#.#...
#..#.##..#
##........
..#....###
###.#.#...
.#.#.#...#
..#.......
.#.#.#.#.#
#........#
#..#.#####

Tile 1753:
###.##.##.
#..#.#..#.
.....#...#
#.........
##.#..##.#
.#.#.#..#.
#......#.#
...#..#..#
#.#..#..#.
#.#..#.#.#

Tile 1609:
..#.####.#
#........#
..#.##.#.#
......#.#.
.....#....
#.......#.
..........
#.........
#..##.....
##...#.#.#

Tile 1237:
.#.##.##.#
...#.##...
..#......#
##...#..##
...#..#.##
.#.#####..
...#.##..#
#....#....
...#..#..#
#..#...##.

Tile 2267:
.#..#..###
...#.#...#
..#..#..##
##....###.
......#..#
..#.......
....#.#.#.
....##...#
###.....##
..##...#.#

Tile 1777:
...#...###
.......#..
##.#..#..#
.........#
#....##.#.
#.#..##.#.
.#..##.#.#
....#.#.##
.###..#.##
......##.#

Tile 1993:
..#.#..##.
#..#.#.#..
.#.#...##.
.......#.#
###.#.#...
#..#....#.
#...##...#
#...##...#
......##..
##.#.###.#

Tile 3571:
.###.#...#
...##.....
##..#.....
..#...#..#
......#.##
..#.##...#
##.#..#..#
#.###...##
..####....
..#.#..#..

Tile 3581:
#.#.#.#.#.
#.#.......
#.#....#..
#.###...#.
#.........
.........#
.#.......#
##........
#.##.....#
##.#..##.#

Tile 2383:
#..##..#..
..#...#..#
#...#.#.##
#.##.....#
....#..#..
...#.##..#
...#.....#
..###.#...
....##..#.
#...###..#

Tile 2591:
##...##.##
..#..#.#..
...#.....#
#.###.....
.....####.
...###.#..
..#....#..
#.........
....##...#
###.###.##

Tile 1153:
##.#...#.#
###......#
#....#.#..
....#...##
#...#...#.
..##....##
#..#...#..
#........#
.#..#.####
..##..#...

Tile 1489:
#.#.###..#
##.#.#..##
.#..##..#.
....#....#
#.#..#.#..
.#.......#
#......#..
##.....#.#
....##.##.
#....#####

Tile 1193:
##..###...
##.#......
......#..#
##......#.
#....#####
.#.#....##
...#..#..#
#..#......
##....##.#
########..

Tile 1163:
##.....###
.#.......#
#...#.##..
#..#...#..
...##.#...
.........#
....#...##
.####.#...
#.#.......
.#..##.###

Tile 2137:
##..#..#..
.#.#...#.#
#........#
#.........
..#.......
#...#....#
..##.....#
#....#.#..
..#......#
..##.##...

Tile 1559:
#..##..#.#
.........#
#..##.##..
#........#
#..#..#..#
....#...##
...#.....#
#......#..
.#........
###..####.

Tile 2521:
.###..#...
#..#......
....#.#.#.
#.....#.##
.#..#...#.
.........#
##...#...#
#...#.....
#..##.#.#.
##..######

Tile 1171:
..#####.##
..##.....#
.....#....
#.#.....##
#.#...#..#
#.##..#..#
#####....#
#..#.##...
##.......#
#.#.#.....

Tile 1361:
..##.###.#
.....##..#
###....#..
..........
#.......##
#..#.....#
#.........
.#.....#..
.......#.#
..##..#...

Tile 1259:
#.##..####
#...#..#..
..###....#
....#....#
...###....
...#..#.##
#.#.##....
##..##...#
##....#..#
#.###..##.

Tile 1987:
##.###..#.
.....#.#.#
#......#.#
#......#.#
..#.#.....
.#..#....#
.####.##..
..##.###.#
#........#
.....#....

Tile 2777:
#.#.##..#.
#...#.#..#
..#......#
...##.....
#.#......#
#........#
#....#....
.......##.
.....#..##
#..##.#..#

Tile 3673:
...###.###
###..#...#
..........
..#.#.##..
#.#..#....
#......#.#
..##......
.#...#.#..
#......#.#
##..#..##.

Tile 2819:
#.##.#####
#.....#.#.
####...#.#
.....#...#
#...#.#.#.
#..#...###
......#..#
....#.#...
.....#.#..
..#######.

Tile 1699:
###....#.#
......#..#
#.........
......#..#
#...#.##..
...#.....#
.........#
...##..#.#
#.........
.#.....##.

Tile 2351:
.#####.#.#
##....##..
###...#.#.
..#.#....#
.........#
.#.#.....#
..#...##..
#..#.#....
........##
###.#...##

Tile 1399:
.###...#..
#.#......#
.##....#.#
...#..#...
#.#.#..##.
#.......#.
#....#..##
....#...#.
#.....#..#
..#..#.#.#

Tile 3793:
##..####.#
.##...#..#
#.....#..#
#...#.....
.....#...#
..........
#....#...#
...##..#..
.#..##....
....#.#.##

Tile 3701:
.....##.#.
#..#.##...
........##
.........#
#..##...#.
#...#.#..#
..#..#.#..
#.....####
#...#..##.
...#..###.

Tile 2707:
#.###.#.#.
#......#..
####...#..
..........
#.#..##..#
#####..#.#
#..#..#.#.
###......#
#..#..#.#.
....#.#...

Tile 3559:
##.#.##.##
#....#...#
...##..#.#
..#.##....
.....#....
..##..#.##
....#.....
......##..
...###....
#.##...#..

Tile 2381:
..#...#.##
..#...#.#.
###...#...
##..##....
....#.##.#
...#......
.#..#....#
..........
##....#..#
###.#.##.#

Tile 1973:
.#...##...
#.#.#...##
#....#..##
#......###
......#..#
.........#
...##..###
.##..#..#.
#.....##.#
...##..###

Tile 3607:
#..#####..
.#.###.#.#
...##...#.
..#......#
.#.#......
##.#....##
###....#..
#...#...##
.#.....#..
#####..#..

Tile 2251:
##..#..#..
#........#
.........#
...#...#..
#.......##
.#.#..#..#
.##.##...#
###...##..
#........#
#.#..#####

Tile 3947:
..#.##..##
###.......
#.#..#....
#.#......#
#.#...#...
##.#.##..#
....##....
...##.#...
#.#.....#.
.##.#####.

Tile 3259:
.#.#####..
.....#.#..
##........
.....#....
#.#.......
.........#
......#...
......#..#
.##.......
#..#.##.##

Tile 3761:
.#....####
.##..#...#
........#.
###.#.....
#.....#..#
..##....##
........##
...#..#..#
...#.#....
###.######

Tile 2473:
#..###...#
##.....##.
#..###....
##....##..
.....#.#..
#.......#.
..#.....#.
......##..
....#....#
#...#.#...

Tile 2767:
##.##.#.#.
.#.####...
..##....##
#...#.....
..#..#...#
#.........
#........#
#...###...
#.#...##.#
.###..##..

Tile 2281:
#...#..#.#
...#......
.........#
#....#....
.......#..
.....##..#
#.#....#.#
##..##...#
........##
#....#...#

Tile 1693:
#.##.#####
......#...
#.#...##..
....#.#.#.
#..#...###
##.##.##.#
..#...####
#..##....#
#......#.#
#....###.#

Tile 2417:
.###.##.#.
##...#...#
#..#..##.#
#...#..#..
.........#
...#.....#
..##.#..#.
#.........
....##....
#.##.###.#

Tile 2879:
..#.#..#..
..##..#.#.
##......##
#..##...##
#.##..#...
#.......#.
.......#.#
##..#..#.#
#...#....#
#....#.##.

Tile 2851:
.......#.#
....#.....
##.###.##.
.......#..
.#..###..#
....###..#
#..#......
...#.#...#
.#.###...#
###.###.##

Tile 3931:
.....##.##
###....#..
#..#....##
#.#.....##
##...#....
#..##..#.#
.......#.#
...#..#.##
.#...#....
##....#.##

Tile 3181:
...##...#.
....#....#
#......#.#
#.###....#
##.......#
.#..#.#..#
###..#.#..
#....#.#..
#.......##
###..###..

Tile 2647:
#..#..#.##
#........#
#..#..##..
.....#...#
....##.#.#
#..###...#
..#......#
#.#..###..
#.......##
####...#..

Tile 2441:
..#..###..
##..###..#
##.##...#.
#.##.....#
.......###
...#...###
#...#.....
.#..####.#
.#.#.#..#.
#.#.##.#..

Tile 3733:
...#....##
..#..##..#
.....#..#.
##........
..#....#..
#....##.##
.#....#..#
.#.##.##..
#.##..#.#.
.#..####.#

Tile 1607:
#.#.#.#.##
#.........
.....#....
.#.#..#..#
..#......#
#.....#...
#.#..#....
......#.##
#.##...#..
..#.######

Tile 3301:
....##...#
#..##....#
....#.....
#.....###.
#....##.#.
#.#...#..#
......##.#
...##.....
.........#
..#######.

Tile 1109:
..#...###.
.#.......#
#.....#...
..........
##.#.#....
#...##....
#.....###.
.#....####
.#....#.##
###.#...#.

Tile 2789:
.#.#.####.
..........
#...#.#.#.
#..#....##
.....##...
.#..##.#.#
........##
#.#.#.#...
......#.##
#...#..#..

Tile 2593:
...#.#.#.#
#......###
..#.##....
#.......#.
...#.#...#
#..##....#
#..##.....
#.##.#...#
#..###..#.
.#....###.

Tile 2539:
...###..#.
..........
.........#
.....###..
.......#..
......#.#.
.#..#...##
#....#.#..
##...#....
.###.#.#.#

Tile 3307:
##.##.#...
.#..#.....
#..#..#..#
#.....##.#
..#.#.....
##....##..
.....#....
#..#.#....
######...#
####.#....

Tile 1613:
###..###.#
...#...#..
#.#..##...
......##.#
......#..#
..##......
#.....#..#
##.#..##.#
###...#..#
#.#..##.#.

Tile 3727:
..#...#.##
#.#......#
#.###...##
..####..#.
#.........
.....#...#
#..##....#
#.#.....##
#....###.#
..#..##.##

Tile 2017:
#...##..##
#..#..##.#
#........#
.....#..##
#..#.....#
....#....#
#....#....
.##.......
.......#..
#..##..#..

Tile 1933:
###.#.....
#.#.####..
..#..##...
##..###..#
###..#.###
..#.#....#
#.........
..#..#.###
#.#..###..
...#.#.#..

Tile 2239:
.#.#..#.##
...#.##..#
#...#....#
#..#......
#....#..#.
#..##..#.#
..#.#.#..#
#....#..#.
#...#.#...
.#.##.####

Tile 3449:
.##...###.
..#......#
.......#.#
..##......
#.###.##.#
....##.#..
........#.
##.#......
...#.#....
..###....#

Tile 2347:
..##....#.
#.........
..........
....#.#..#
.........#
##.##.#..#
......#..#
.#.##...#.
#.#####.##
.#.#....##

Tile 3023:
#...#.##..
.......#..
#.#..#.##.
#...#.#..#
..#.#...##
#.##...##.
......##..
.....#####
.#...##...
#..#.#....

Tile 3541:
###.######
..#.....#.
#....###..
......#...
..#..##..#
#........#
#..#......
###..#.##.
..#....#.#
#..###.#.#

Tile 3709:
##.#.##..#
#.........
.##.##...#
.#..#.#..#
#.......##
......#...
.#..#.....
.......#.#
#..##....#
.##.......

Tile 1657:
#.##.#...#
...#.#..#.
.....#..##
.....#.#..
######.###
##........
......#..#
#..#...#..
.......#..
...#....#.

Tile 1031:
####.#.##.
..........
.#...#..#.
...#.....#
.##.#....#
....#.##..
........##
#..#..#.##
#.##.....#
#.####...#

Tile 3691:
.#.#######
........##
#.#.......
#..##..#..
#.#.##.##.
....###.#.
.#..#..#..
..#.##.#..
.....#....
##.###...#"

type ID = int64
type Tile = { ID: ID
              Raw: char [] []
            }

let parseTile (raw: string): Tile =
    match Array.toList <| raw.Split "\n" with
    | tileIdRaw :: data ->
        let idRaw = tileIdRaw.[5..tileIdRaw.Length - 2]
        { ID = int64 idRaw
          Raw = data |> Array.ofList |> Array.map Array.ofSeq }
    | _ -> failwithf "weird tile: %A" raw

let parse (input: string): Tile [] =
    input.Split "\n\n" |> Array.map parseTile


let add (value: 'v) (map: Map<'k, Set<'v>>) (key: 'k): Map<'k, Set<'v>> =
    match Map.tryFind key map with
    | None -> Map.add key (Set.singleton value) map
    | Some oldSet ->
        let newSet = Set.add value oldSet
        Map.add key newSet map

let getSimpleEdges (tile: Tile): string list =
    let transposed = Array.transpose tile.Raw
    [ tile.Raw.[0]
      tile.Raw.[tile.Raw.Length - 1]
      transposed.[0]
      transposed.[transposed.Length - 1] ]
    |> List.map System.String.Concat


let getAllSymmetriesOfEdges (tile: Tile): string list =
    getSimpleEdges tile
    |> List.map (fun edge -> [ edge; String.rev edge ])
    |> List.concat

let part1 (input: Tile []): int64 =
    let map: Map<string, Set<int64>> =
        input
        |> Array.fold (fun acc tile ->
            getAllSymmetriesOfEdges tile
            |> List.fold (add tile.ID) acc) Map.empty

    map
    |> Map.toList
    |> List.filter (fun (_, v) -> v.Count = 1)
    |> List.map (fun (edge, idSet) ->
        idSet
        |> Set.toList
        |> List.map (fun id -> (id, edge)))
    |> List.concat
    |> List.fold (fun acc (id, edge) -> add edge acc id) Map.empty
    |> Map.filter (fun id edgeSet -> edgeSet.Count = 4)
    |> Map.toList
    |> List.map fst
    |> List.fold (*) 1L


let correctNumberOfConnections (side: int): int =
    //   4 corners * 2 connections
    // + (side - 2) * 4 edges * 3 connections
    // + (side - 2) * (side - 2) inner pieces * 4 connections
    // ==
    4 * side * (side - 1)

type Piece =
    { ID: ID
      Raw: char [] []
      LeftEdge: string
      RightEdge: string
      TopEdge: string
      BottomEdge: string
      Left: ID option
      Right:ID option
      Top: ID option
      Bottom: ID option }

let countConnections (pieces: Map<ID,Piece>): int =
    pieces
    |> Map.values
    |> Seq.map (fun piece ->
        (if Option.isSome piece.Left then 1 else 0)
        + (if Option.isSome piece.Right then 1 else 0)
        + (if Option.isSome piece.Top then 1 else 0)
        + (if Option.isSome piece.Bottom then 1 else 0))
    |> Seq.sum

let tileToPiece (tile: Tile): Piece =
    let transposed = Array.transpose tile.Raw
    { ID = tile.ID
      Raw = tile.Raw
      TopEdge =    tile.Raw.[0]                        |> System.String.Concat 
      BottomEdge = tile.Raw.[tile.Raw.Length - 1]      |> System.String.Concat 
      LeftEdge =   transposed.[0]                      |> System.String.Concat 
      RightEdge =  transposed.[transposed.Length - 1]  |> System.String.Concat 
      Top = None
      Bottom = None
      Left = None
      Right = None
    }
    
    
       
let hasTodo getter p =
    getter p = None
    
let numOfNones (p: Piece) =
    (if p.Left = None then 1 else 0)
    + (if p.Right = None then 1 else 0)
    + (if p.Top = None then 1 else 0)
    + (if p.Bottom = None then 1 else 0)
    
    
let findAndUpdate (map: Map<string, Set<ID>>) (cornerIDs: Set<ID>) (piecesSoFar: Map<ID,Piece>) (piece: Piece): (Piece * Piece list) =
    (piece, [])
    |> (fun (p,acc) -> 
            if hasTodo (fun x -> x.Left) p then
                match map.TryFind p.LeftEdge with
                | Some ids ->
                    let maybeOtherId =
                        ids
                        |> Set.remove p.ID
                        |> Set.toList
                        |> List.tryHead
                    match maybeOtherId with
                    | Some otherId ->
                        let newP = {p with Left = Some otherId}
                        (newP,  acc)
                    | None ->
                        if Set.contains p.ID cornerIDs then
                            (p, acc)
                        else
                            failwith "try some symmetries Left"
                | None ->
                    (p, acc)
            else
                (p, acc)
        )
    |> (fun (p,acc) -> 
            if hasTodo (fun x -> x.Right) p then
                match map.TryFind p.RightEdge with
                | Some ids ->
                    let maybeOtherId =
                        ids
                        |> Set.remove p.ID
                        |> Set.toList
                        |> List.tryHead
                    match maybeOtherId with
                    | Some otherId ->
                        let newP = {p with Right = Some otherId}
                        (newP,  acc)
                    | None ->
                        if Set.contains p.ID cornerIDs then
                            (p, acc)
                        else
                            failwith "try some symmetries Right"
                | None ->
                    (p, acc)
            else
                (p, acc)
        )
    |> (fun (p,acc) -> 
            if hasTodo (fun x -> x.Top) p then
                match map.TryFind p.TopEdge with
                | Some ids ->
                    let maybeOtherId =
                        ids
                        |> Set.remove p.ID
                        |> Set.toList
                        |> List.tryHead
                    match maybeOtherId with
                    | Some otherId ->
                        let newP = {p with Top = Some otherId}
                        (newP,acc)
                    | None ->
                        if Set.contains p.ID cornerIDs then
                            (p, acc)
                        else
                            failwith "try some symmetries Top"
                | None ->
                    (p, acc)
            else
                (p, acc)
        )
    |> (fun (p,acc) -> 
            if hasTodo (fun x -> x.Bottom) p then
                match map.TryFind p.BottomEdge with
                | Some ids ->
                    let maybeOtherId =
                        ids
                        |> Set.remove p.ID
                        |> Set.toList
                        |> List.tryHead
                    match maybeOtherId with
                    | Some otherId ->
                        let newP = {p with Bottom = Some otherId}
                        (newP,  acc)
                    | None ->
                        if Set.contains p.ID cornerIDs then
                            (p, acc)
                        else
                            failwith "try some symmetries Bottom"
                | None ->
                    (p, acc)
            else
                (p, acc)
        )

let toDotEdge (id: ID) (otherID: ID option): string =
    match otherID with
    | None -> ""
    | Some otherID_ -> sprintf "x%d -> x%d" id otherID_
        

// suitable to pasting into GraphViz
let part2FindConnections (tiles: Tile []): int =
    
    let map: Map<string, Set<int64>> =
        tiles
        |> Array.fold (fun acc tile ->
            getAllSymmetriesOfEdges tile
            |> List.fold (add tile.ID) acc) Map.empty
        
    let cornerIDs: Set<int64> =
        map
        |> Map.toList
        |> List.filter (fun (_, v) -> v.Count = 1)
        |> List.map (snd >> Set.toList >> List.head)
        |> Set.ofList
        
    let side: int = isqrt tiles.Length
    let correctConnections = correctNumberOfConnections side
    let pieces: Map<ID,Piece> =
        tiles
        |> Array.map tileToPiece
        |> Array.map (fun p -> (p.ID, p))
        |> Map.ofArray
    
    let isNotDone _ p =
       (p.Left = None || p.Right = None || p.Bottom = None || p.Top = None)
       && (if Set.contains p.ID cornerIDs then numOfNones p > 2 else true)
    
    let rec go (previouslyDone: int) (piecesSoFar: Map<ID,Piece>): Map<ID,Piece> =
        let doneConnections = countConnections piecesSoFar
        if previouslyDone = doneConnections then
            failwith "cycling, gotta try something else"
        else
            if doneConnections = correctConnections then
                piecesSoFar
            else
                match Map.tryFindKey isNotDone piecesSoFar with
                | Some pieceID ->
                    let piece = piecesSoFar.[pieceID]
                    let (newPiece, otherPieces) = findAndUpdate map cornerIDs piecesSoFar piece
                    let newPieces: Map<ID,Piece> =
                        otherPieces
                        |> List.fold
                            (fun acc newOtherPiece -> Map.add newOtherPiece.ID newOtherPiece acc)
                            (Map.add newPiece.ID newPiece piecesSoFar)
                    go doneConnections newPieces
                | None ->
                    piecesSoFar
                    |> Map.toList
                    |> List.map (fun (_,p) -> sprintf "\nx%d\n%s\n%s\n%s\n%s" p.ID (toDotEdge p.ID p.Left) (toDotEdge p.ID p.Right) (toDotEdge p.ID p.Bottom) (toDotEdge p.ID p.Top))
                    |> List.filter (fun s -> s <> "")
                    |> List.toSeq
                    |> System.String.Concat
                    |> printfn "%A"
                        
                    failwith "bailing out to GraphViz to connect these together"
    
    go (-1) pieces
                    |> Map.toList
                    |> List.map (fun (_,p) -> sprintf "\nx%d\n%s\n%s\n%s\n%s" p.ID (toDotEdge p.ID p.Left) (toDotEdge p.ID p.Right) (toDotEdge p.ID p.Bottom) (toDotEdge p.ID p.Top))
                    |> List.filter (fun s -> s <> "")
                    |> List.toSeq
                    |> System.String.Concat
                    |> printfn "%A"
    -1
         
let transpose    = Array.transpose
let flipY        = Array.rev
let flipX        = Array.map Array.rev
let rot90        = transpose >> flipX
let rot180       = flipX >> flipY
let rot270       = transpose >> flipY
let invTranspose = transpose >> flipX >> flipY
    
let symmetries tile =
    [ tile
      flipX tile
      flipY tile
      transpose tile
      rot90 tile
      rot180 tile
      rot270 tile
      invTranspose tile
    ]

let exampleConnections: ID[][] =
    [| [| 1951L; 2311L; 3079L |]
       [| 2729L; 1427L; 2473L |]
       [| 2971L; 1489L; 1171L |] |]

let part2Connections: ID[][] =
                               [| [|3701L;2521L;3167L;3181L;3947L;3023L;2579L;2633L;2239L;3989L;2269L;1399L|]
                                  [|2659L;1291L;3169L;1973L;2593L;1657L;2113L;2591L;1901L;3499L;1699L;1109L|]
                                  [|3329L;3547L;3943L;2677L;2789L;2267L;1181L;2851L;2539L;3709L;1787L;1609L|]
                                  [|2137L;1753L;3559L;1801L;1427L;2647L;1187L;2969L;2297L;1259L;1721L;1031L|]
                                  [|2251L;3511L;3691L;3323L;3163L;1171L;3259L;2083L;1481L;1777L;1033L;3079L|]
                                  [|2281L;3457L;3449L;2879L;3571L;1117L;3911L;3389L;1237L;2707L;2531L;1279L|]
                                  [|1039L;2917L;2203L;2767L;3607L;1693L;2819L;3301L;3191L;1453L;2897L;1823L|]
                                  [|2351L;3517L;1979L;1933L;3407L;2131L;2377L;1607L;1489L;2411L;1613L;1321L|]
                                  [|2777L;3719L;1153L;1361L;3257L;2749L;2347L;1559L;3541L;2017L;2383L;2473L|]
                                  [|3767L;1997L;3877L;3673L;2087L;2711L;3307L;1123L;3761L;3793L;3347L;1231L|]
                                  [|2237L;1409L;2549L;1663L;2417L;3067L;2381L;2731L;2729L;1993L;2389L;2161L|]
                                  [|3209L;1193L;2441L;2063L;3931L;3581L;3727L;1987L;1163L;3733L;2963L;3803L|] |]
 
let removeEdge (raw: char[][]): char[][] =
     raw.[1..raw.Length-2]
     |> Array.map (fun row -> row.[1..row.Length-2])
     
let topAgrees (top: char[][]) (bottom: char[][]): bool =
    top.[top.Length-1] = bottom.[0]
    
let leftAgrees (left: char[][]) (right: char[][]): bool =
    Array.map (fun (x: char[]) -> x.[x.Length-1]) left = Array.map (fun (x: char[]) -> x.[0]) right
     
let fixTiles (map: Map<string,Set<ID>>) (connections: ID[][]) (tiles: Map<ID,char[][]>): Map<ID,char[][]> =
    connections
    |> Array.mapi (fun y row -> Array.mapi (fun x id -> ((x,y), id)) row)
    |> Array.concat
    |> Array.fold
        (fun accTiles ((x,y),id) ->
            if x = 0 && y = 0 then
                // orient corner and neighbouring tiles such they share edge
                let rightID = connections.[0].[1]
                let bottomID = connections.[1].[0]
                
                let (goodCorner: char[][], goodRight: char[][], goodBottom: char[][]) =
                    [for cornerSym in symmetries accTiles.[id] do
                     for rightSym in symmetries accTiles.[rightID] do
                     for bottomSym in symmetries accTiles.[bottomID] do
                             yield (cornerSym, rightSym, bottomSym)]
                    |> List.find (fun (corner,right,bottom) -> leftAgrees corner right && topAgrees corner bottom)
                accTiles
                |> Map.add id goodCorner
                |> Map.add rightID goodRight
                |> Map.add bottomID goodBottom
            else if x = 0 then
                // check against top
                let properlyRotated =
                    symmetries accTiles.[id]
                    |> List.find (fun chars -> topAgrees (accTiles.[connections.[y-1].[x]]) chars)
                Map.add id properlyRotated accTiles
                
            else if y = 0 then
                // check against left
                let properlyRotated =
                    symmetries accTiles.[id]
                    |> List.find (fun chars -> leftAgrees (accTiles.[connections.[y].[x-1]]) chars)
                Map.add id properlyRotated accTiles
                
            else
                // check against top and left
                let properlyRotated =
                    symmetries accTiles.[id]
                    |> List.find (fun chars -> topAgrees (accTiles.[connections.[y-1].[x]]) chars && leftAgrees (accTiles.[connections.[y].[x-1]]) chars)
                Map.add id properlyRotated accTiles
        )
        tiles
 
let part2Rest (connections: ID[][]) (tiles: Tile[]): int =
     let map: Map<string, Set<int64>> =
        tiles
        |> Array.fold (fun acc tile ->
            getAllSymmetriesOfEdges tile
            |> List.fold (add tile.ID) acc) Map.empty
     let tilesWithEdges: Map<ID,char[][]> =
         tiles
         |> Array.map (fun t -> (t.ID,t.Raw))
         |> Map.ofArray
         |> fixTiles map connections
         
     let tilesWithoutEdges: Map<ID,char[][]> =
         Map.mapValues removeEdge tilesWithEdges
         
     let ts = tilesWithoutEdges.[tiles.[0].ID].Length
     
     let connected: char[][] =
         connections
         |> Array.mapi (fun browi brow ->
                                  Array.mapi (fun bcoli id ->
                                  tilesWithoutEdges.[id]
                                  |> Array.mapi (fun trowi trow ->
                                      Array.mapi (fun tcoli char ->
                                          ((browi * ts + trowi,bcoli * ts + tcoli ), char) ) trow) ) brow)
         |> Array.concat
         |> Array.concat
         |> Array.concat
         |> Array.sortBy fst
         |> Array.groupBy (fst >> fst)
         |> Array.map (snd >> Array.map snd)
     
     (*
        |                  # 
        |#    ##    ##    ###
        | #  #  #  #  #  #   
      *)
         
     let mask =
         [(18,0)
          (0,1);(5,1);(6,1);(11,1);(12,1);(17,1);(18,1);(19,1)
          (1,2);(4,2);(7,2);(10,2);(13,2);(16,2)
         ]
         
     let hasMonster (cs: char[][]) (x: int, y: int): bool =
         mask
         |> List.map (fun (cx,cy) -> (cx+x,cy+y))
         |> List.forall (fun (cx,cy) -> cs.[cy].[cx] = '#')
         
     let indexesToTest (cs: char[][]): (int * int) list =
         [for x in [0..cs.Length - (* mask cols *) 20] do
              for y in [0..cs.Length - (* mask rows *) 3] do
                  yield (x,y)]
         
     let findMonsters (cs: char[][]): (int * int) list =
         indexesToTest cs
         |> List.filter (hasMonster cs)
         
     
     let monsterTopLeftCoords: (int * int) list =
         symmetries connected
         |> List.map findMonsters
         |> List.find (fun x -> x.Length > 0)
     
     let monsterCount = monsterTopLeftCoords.Length
     let monsterTileCount = monsterCount * mask.Length
     let hashCount =
         connected
         |> Array.concat
         |> Array.filter ((=) '#')
         |> Array.length
         
     hashCount - monsterTileCount
     

//let main = realInput |> parse |> part2FindConnections
//let main = exampleInput |> parse |> part2Rest exampleConnections
let main = realInput |> parse |> part2Rest part2Connections
