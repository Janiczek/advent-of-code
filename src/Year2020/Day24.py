from typing import Tuple, Callable, TypeVar
from adt import adt, Case

input1 = """sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew"""

input2 = """swswswswneswswwwswnewswswweswnwsww
nwwewenenwnenwnwnwnwneswnwswnwnwswswnw
seneswwwswwnenwnenwswswswswsewseeww
esenesenesesesewseseswnesesweesesesese
seseseeseenesewseenwsesewswwsesenwse
eswenewnenenewneneneneenenenenwnene
nwseeseseeseseseseewseesenwseeesese
nwnwswswswweneseswwswwneswswswswswsw
senenenwnenewneneneseswneeeenenenew
eswnwnesenwnwnenwnwnwnwwnwsenwwswswenw
wswneswswseesewswseswswswnenesenenwswswne
seswseswneswswswwnwswwswseneswswswswsee
neseseswnwesesesesewswsesenesenesesesw
wwswwwsweswwwww
seswwnwneenwswneenwewneneneseenee
eswseenweswewseneeeeseeswnwnwse
swsweswswswnweswswneswwswnwswwwswswswsw
nwwnwnwnwnwnwnwnwwswwnweneswwenwnw
sewswwwwwwwweswwwwneswwnwswnw
nwneeneeneneseneseneneneneneneswnenwnene
swnenenweeneeeseswweneenenenenene
nwnwnwsenwewsesenwenwwwswwnwnesenw
swnwswswswswswsewseswnewneseswswswneesw
wwweswwswwsewswswwwwnwwesewwne
swswsweneswswswenwswnewnwswwneswseswswe
nwnesewwnwwnwwwsew
enwwwnenwnewwwwswwseeswewe
neswseswswsenwseswnwseswse
senwseneneneneneeseneswnenenenenwnenwnw
swseneswwseenwsenwneneswnewnenewene
ewwwwwwwewwwnwswwwwwwwne
eswswenwsenwnwsenenw
nwneseewseswwsewswnwnwnwwwwnewww
nwwwwwwwwewnwwswnew
neeeseeesewseeeeeeeewe
wweswnenwwnewnwnwwwewsewnwwsew
swwswwwswwewwswswnwsweswnwwwww
eseswneenwneenweseenweeseeeenw
neneswneneneneneneneneneswnenwnwnw
nwnwwnwewwswwnwwwnwsewnwwenwnw
seswswswnwswwwwswwswneswwseswnw
nwseswenwwneneneseswneenwnenenwnenwnenw
nwnenenenesewwnenwnwnesenwnwnwnwnenwnwnw
wseeeneseeeseseesesenwseseswsesese
sweseswnwseseseswseswseseseswswswse
ewwwswswsenwewnewwwenwwwnwww
nwwsewnwnwnwneswwnwesewnwwnwnwnwnwne
nenwnwnwnenenwnenwnwswswnwneenenwnwnwne
esenwneswsesesweeeeweseseeseseswnw
swswswswwsesweseswswswsw
swsweneswwseswswswwswsenewswwswseswnene
nenenewnwnenenwenwnesenwneseswnwnwnwne
nenenenewneeseneneswneneneswsenenenwne
nenenwnenwswwnweesenwsenwnwnwseswnwnwnw
seseeenwsenwswsesenwseeseeseeseswese
swneeseseneswswnwswnwwwwswswnee
seeseseseneseseswsewseenenwwneseswse
wswseseenwwswnewenwswswwwnewwsw
nenewswnwnenwnwneeneswseeneswnenwnenesene
nwswsenweweseeneseesesesesesesesee
seneswswwesenenwswseswnwseseseseseswne
wwnwwweswnwnwwweneseewnwsesenw
swswneneneneenewnesenewweenweswne
eseneseneseenenewwswseweenweenenene
nwwnwwnwsenwnwnwnwwnwwnww
swweswsewewwwwwwwwwwnewne
swswnewseseseesenewseesesesesesenwese
swswnenwnenwnwnwnwnenenenwnwwnenwseene
wnwwwwnwnwwnwwnwnwwwnwnwsenwe
nwseswwwnenwwwwesewwwwwwwnenwnw
nwswswnwswwswwnenenwnwnwnwnewnenw
seswswswnwswswswswswswswswswswswsw
neneseeneswnwnenwnwsenwnenenewne
wwwwwnenwnwnweswwnwsewwwesese
swwswswwwwseswswenwnewswwswweswnw
swwswseswswseeswswswewswswnwswswese
nwnwewsenenwnesewwwwswnwnenwswsee
seseseenwwswseneneseswswnwnewswnwsenwse
wwswwnwwwwsewwwenewnwswnwwwe
swwwwswwswwswneseswswswwnewswwse
nenenwneeswnenesweneneneeneneswsewesw
seswswswseswswswswsweenwswswwseswwsw
swswswsewneseesenwseseswseswswswnwsesw
nwwnewnwnwswnwnwswnwsenwwnwenwnenww
wswwnwwwswwwwswe
eewneeeeneeswsee
wseneseeseewseseseswsewse
wwswnwnwwnewnwsewenenwswwewswwne
seneesesesesewswswwnwseswswseseswseswsw
ewnweeeseneseeseseeneseswwwee
wwwwswwsewwwwewwwwwswnwwne
swsewswswswseswseseeswsenwseswswswsenw
nenenenenesenenenenenenewenewneswsene
newnewewwnwnwnwsewwswnwwwwesww
neseeeneneeewnee
wsenwwnwenwnwwnwnwnwwnwnwwnw
nweswenwseseswwseeenwseenwswsesewsw
wwwswwwwsewwwwwwwwne
nenwnwnwnenwnwnwnenwwnwnwnwnwnwe
eeeeeweneeseeeesweeeeew
seneseeeeewewnweseeseseseeseese
swenenenenwesweewneeeneneeeeeee
wnenwseswnwwwsenwnwwnwnwwnwwnwnwnwnw
sewwwswnwwnwwwswwnewwewwwsw
swwwwswswwswswwwswnew
esweeneeswenwnweesweneeeeswee
eenweeseneeeeeeeeeseeenenwsw
nwneswweeneseswnwswnweeesesesenwe
neneneeswnwnwnwnenenwnenene
wseewseeseseneswsenwsenewsesweseswswse
eseswneneseneenenewswwnwnenenesenenenw
seswsesesweseswseswseswwsw
nwnenenenenwnenenwsenenwwsenewnenenene
eneneneneswnenenenenenenenwneneneswnwnw
neneeneneenwnenenesweneneewsweneene
swnwnwneswnwnwseenwnwnenwenewnwnwnwwnw
swswwnwwswnwwseswewswneseneswewneswe
neswswnewwswswwwswwsewww
nwneeenwswnwsenwnwnwnwnenwswnwnwsenwnw
neeswnwnwwnwseswesweneswnw
swnenwenwnwnwnwsesenwswnwnwnwnwnwnwnwnw
swseswswnenwwwwseswwwwsw
seswneseswseswswswsewesenwsenesewsesw
eseeneswseenwewseewneeeene
swnenenenenenenewwneneswnenenesenesenw
swnenwnenwnwenenenwnenenwnwnene
eswneenwwnwwwneewneneneswneseswee
neseneseneewneeeweewnwneeseee
nesenenenenwnwnenwswnwnenwnenenwnw
seswsesesenwseeeeseseeesesewseenw
eneswsewneeswnenweswneeswnewwswseene
eeseseseswneswseseeeeeeeeewne
wwwseswnenwwnwnwenenwnw
sesenesweeeeeweeeweeenwwe
wsewnwwsewwewwnew
esesenwnwwwneswwswsenwnwnwseswneswwew
nenwwnwnwnwswwenwwwenwwwnwnwwnw
seeeneeneeseeswnenenwsweweewee
swswswwswswswswwswswwswneswswsw
nweseneweseeswwnwwswswnw
wnwsweswseswswswswseswne
swnenenwneenenenenenenenenenenenene
swswswswswseswswwswswswwnwwenwnwseswesw
wswwswswwswswsweswewswwswnwnesw
neneeewneneeswnewneneeseseswewnw
seseswseeesenwsesweseneseeseeesese
nenenenwnesewneswneneseneenewnene
neneneneneneswwneneneneneesenenenenene
swswswnwswnwswsweeswswseswswswswswswnw
sweeseseneeeseeswnewnwnwwewsee
neswsenwnewwnwwwwwwswweseenenww
nwwwwnwwwsewnwwswnewwwnewww
nwswsweswswneswseswswswswwneseswwnesww
wsewwnwwwwswnewwwwsewswwwsw
nwwwwwsenewneewwseenewwsesw
nwnenwwnwwnwwnenenwnenwenesenwenwne
nwnenwnwsewnenwsenwnenenwnwnenenenwnene
wswswwwenwswenesewwnenwe
wseswneeswneseenwnwsenwswewwweswesw
swnwseseeeeewseswnwseneseswsenwsesene
sweneswnwnwswsweneswnwswneswneswsweswsw
eeeswnwswneeneenewswnewneneeee
senwwnenwenwnwwsenwnwnwnenw
esesenweseeeeseseeesesese
seeeeseneseseeeswseeeeseewsene
wneswwwwswswwwewwnesesenwwwse
wnwnwwwsenwnwswnwnwnenewnwnwwnwwnw
senewsesesesesesesesesesesese
eenweswneeenee
nwnenwwnwnwnwnwwwseenwnwnwwnwnwnwswnw
esweneseeneeneeneneneneneswnenewnee
nwwwwswwewnwnwww
neneeneewneeneneneswnwnesenenenenwnesene
wneswsesenewswwwwwwwnwswwwseww
wesenesewswsenwswneswswseswneesesese
sesesenwswsewnwsenwswseswesenenwseeseew
wwnwswwneswwweswwwwwwwswww
nenenwnesewwswswewsenwswnewwswew
wwwnwwwnenwwwwwnwswnwnw
nenwswneneneenenenenwnenwnwnwnenwnw
swenwswewswswswewnwwswnewswseswne
swsenweeseswenwseseswsenwsesewseswswse
eeeweswnwnenenweeswneeeseneeee
swwswneseswnwseneswnewswweseswnwww
swswswswnenwswswseswseswswswneswwseswsw
enwsewsweeseseseneweeseswesenwese
nesenweneneneneneswnwnwnenwnenwnenwnene
seswseeenwsesenweseseseseseneseeseeswse
senenwsewnwnwnenwewnwswwneswnwnwsweenw
neeewnenweseseseeswnesweneneewwse
eeenwswswseneweseneenweneneneew
swswneneswnenenenwnenwnenenenwnwnwwnwe
swsweswwswswswwnwwwweswswswwswnw
wewwewwwwnwswswwwwswwswsww
swwnwwswnweseswnwswseweenewwww
neswswswseswswseswswswseswwswswswsesenw
eneeneneneewneneneeneneenesew
nwnwnwnwnwnwnwsenwenenenwnwnwnwwnwnwswne
weneswnwsenewseseswseeseene
eseseweneenwnweseeseseneneweswswe
swswswswseswswsenwnwseswseeswswsweswnw
sweswwsweswswwswswsweswswwnwswnwenw
eeeseseeenwsewswesesewsenwsenesw
seneswseseseseneseseseweesesesw
ewnwnwwnenwwnwwnenwwnwsewwnwswww
senweeswseswneweesesesesewnwnwsee
wseseseseseseeneseese
neswneswewnwswwwwwwwswwwseewnwse
sewwsesesewnwswswswseeneeesesewse
sweswwneseneswswswswnwswnesene
eesweeenenweeneeeswenwneeeene
eseseeneweeseeeeneenwneeenwne
neneneneneswneenenwnewneneneneswnenenenee
nwnesewwswswseswnwwneswswwswwsesesw
eeneeswesenwewenwneneeeseenwesw
nenwneneneneswnenenenwnwnenw
wwseewnewwneenwwnwswsewseenwse
wsenwwnwwswwwwwswnweewnenwsee
nweseswseseseeeseeseeseeesenwsese
neneswseesewesenenenwneswwswnewswswsenw
wwneseenwwsenwnwnwneswswewnwswwnwne
eneneeneneeneesw
nwnenenwenwnwseeenwwswnwnenwnenewnw
swsweswswsewewswswnwnwwsewwe
swseneswswswswswswsesewswswseseswswnesw
eseseesenwsenwseseeeweesesweeene
neswnwsewsweseswseseswsesesenwnwneseseswse
eeneeeseeseenwneeneeeeeewne
nenwswewnwsweneswwenwswneseseneenew
weeswwwwnenwswnwsenewnwswwnwnwenw
wwnewswswswwnwseswwwswswwwnwsew
newsenwwneswwnwwnwswseeswseswsenwww
swswwsweneswwwnwnee
nwswswswswswswswswnwnwswenwseswswseeswsw
nwnwnwnewswwswnwnwnenwsenweenwnwnwnese
nwenwseseseswswswswswswenwseswnwewswswse
swneswwewsenwnwnweeenenweeseese
nwwwwwswnwesenwwnee
nwnwwwwnwwnwnwsenw
nwseseseswswneseswswnwseseswsweswsweswsw
enweswnweswnweneeeneneneeeswee
sesenwseswnwswswnenesesesesweseseesesw
swseswsewnwnwwneswswswseswswneseswwsw
nenwnenwnwnwnwnesenwnwnwnwswnene
swwenweesenesweesenweseeeeee
swnewsesenwswesweeneneesewnw
swneweswsenwnenesese
nwnweseeseseseswseewseseenenwsee
neswswneseeeeenenwwneeenenwneseneene
swswswsesesesenwsesesenwseswsesese
nwswnwenwnwnwnwnwnwnwnwnwnwsenwnw
eeneesweeeeeeseswnweeneneewe
eeenweseseeeswnwwnwnenwswnewsenenw
neswswswswseseesweswsenwsesenwnwswswsesew
eseeeseenweseeeeseese
swwwswwswwswwsewwswswnwsww
swswwsenwswwswswswswwwswew
eeeseeeseswewneeswneee
sweseswswenwnwwswswnwnenwwseewwwsee
wwnwwwwnwnwwwwewwenewnwswww
sewneswnwseeneswwwnewnwenewseswwnw
seswswswswwswswswnwsweswswswswseswswe
seswswswswswnwneswnwswswsesesenese
eenweseeeseeeeneneeeweseesw
swnwseswswnweneswenwnewenenwnwswnenese
wweenewnwsewnwwnwnenwswseweesw
wwseneeneneneneseenwwswsewwesee
wwsenewwswswwwswwwwswsw
nwwswwwnwwwnenwnwwnwnenwwswwnesw
eneseeweseesesesweewnwenwwswe
eneneeeweneeseeeenesenenenwne
neeneeeeesweseweeeeeewee
swsewnesenweenwenwnwneseseswnwnenene
sewseswseseswseseseswswesenwneswsesese
nwwwnewsewswswwnwseewwwsenewne
enwneseeeeswswneeenwnweeewwsw
wnwswswswswswsweswsw
nenenenwnesenwnenwnwswwswnene
swseseseseenwesenwsesewseesewnwseenw
eswenwsweeeenwneswseeeenwwnee
sweeswswwswswnwwnwswseseeswsesw
nweenwswsesenwneswnweeeeseseswswsenw
wnewseenwnwswneseeeeswnenwneene
nwsewnwnwnenwnwnwswenwnwnwnwwnwnwnwse
swswswswswswswnwswswswswseeswnwseswswsw
senwseswseeseneseewnwnweeseesesesesese
eeseeeenweeeneeeeneeeew
seswsenwswseesesenwswsesweswneseswsesese
nwwnenwnwwwnwwwsenwwnwnwsewnwnewsew
swneswswswswnwwswswswseswswsweswseenw
swwnenwnwneneenenenwnenwnenenenwnwne
ewneswsenwnwenenenwswseeesewnwsww
wswswenwwnenwnwseeswwnweswswnweeswse
nwnwnwsewnwnwnwenwswwsewnwenwnwwnw
neneswneneneneneneeneenenwnenewnwneenesw
eeeseseeweeeeenee
nwnwnwswsenenwenwnwnwnwswnwnwsenwnwsweene
wwsewwwwswwnewwnewwswwswwsw
nesesewwneseneseweeenwsweenwswwse
eeenweesweeewweeeeneeeee
eseswnwwsenwswneswsesweseneeneneswwwsw
wnwnwnwnwewnwnwseenwwnww
enwsenwseseeneseeseseseeswnwseesesee
neneseseseeswswwseswnesesewswnwnwwee
neenwnwnwnenwsenwnwnwsenwnenenwnwneneswnwnw
senewnwnenweeewnenwnenenwswnwnenwswnwne
seseswseswsesesesesenesewseesesesesew
nenenenwwnwneeneenwswnwnenwnwnenewnee
wwwwwseswneewnewneswnwewswsenw
nwnenwnwnwnwenwnwnenenwnwsenwnwwwnwne
wswwsenewnenenesewneneseseseewnewsw
nwnwsenwnwnwnenwnwnwnwnwnwnwnw
nwswswnwnenenwenenewneeeneswswnenwwnene
sesenwnwenwnwwnwnwnwnwnwnwnwsenwneswsenw
nwneneneswnenwnwnwnenenwnenwsenwnene
seneswneewswnenwwneeeeneeneneewe
seswswswswswneswswswswwweswwswnwswwne
swnenwnwwnenwnwnwsenenwweesenenenew
swsewwnenwnwwwwsenwwwewnwwsenew
eeeeeneeneeweneeeneenwnewswene
nwnwnwnwwnwwwwnwwwwnewnwwswneswe
swwnwsweneseswwswnewswswnwswseswnwseswe
swswswswswswswswswswseswswswswneewnwww
senwnenwsenenewnwnwwsenenwnwnwnwenenw
wswnwenwenewnwwwwwnwnwwswwwsw
swseseswswseswseneswwseneseswwseswsesese
eeeswsenewenweseeeeseeeenesw
enwnenwneswwewnwsewnwnenenwnwse
nwnenwnenwenwnwnenwnwnwswnwnwnwneneswnw
sweswseswswnwswswswswswswseenwswsewsw
wnwnwewnweenwnwewnwwsenwweswnw
senenwnwwnwnwnwwnwwwnwnwnwnwnwnwnwsesw
nwnwnwneswnwnwenwnwnewnwneenwnwnwswne
seesenewnwneswnenenenenenenenesewnene
seswwswnenwnwseseswseswswswseseweneesw
swwswenwwnwnwwewwwnwwnwnwwe
nwnwnenenwnwsenenwnwsenwnwswnwnwnwnwnw
wewsesenwswnewnwnwwnw
nesenenewneesenenweneweneeneeneeswne
senwwsenwswswnwneesewwnwneenwwnwswe
nwnwenwnwnwswnwnwnwnwnwswnwnwwenwnwnwnw
nwnwnwswnwnwneewnesenwnwnwnewnenwnenwnwe
wseseenwswseneseseneeeseeeeesenw
nenewwsewwenenwwsewswwnwwswww
nwwwnwwnesewewwwwswwewwseww
eeswseneswwsenwesesweswwsenwnwnwnese
wnwwswwewwwwwwwsenwnwwnwwsenw
nwnwnwnwnwneseneenwnenwwneenenwnwwnw
nwnwsewswnesenenwenweneseeneswwnenwwne
nwseeneneneneeneneneneneswwwwewswse
nwnenwnenenenwwswneneswnwneneneeenenwne
swswswsweswnwwswswswsweswneswswswswsw
seswseseswseswseswswnwseswswswseneswew
senwwwweswnwneswenwwswnwnwnenwwse
swswswswsewseswsewsesesesweneswsesesw
enenewneneneeeewneneseeneeneseene
nwnwnwnwnwwsenwnwsenwnwsenwnwwnwnenwnwnesw
nenenenwswnwneneneneneneneenwsenenenenw
nwnenwnwnwnwnwenwnwnwswneswnwnwnwnwnwsenw
neenenwseswnwseswswnwwnenwnwnenenenwswsew
sewnenwnenwenesenwwnwsenenwwne
wswnwwswwnwnwnwnenewnwnw
senwswneeswswswswwwswswswwsweswwsw
esewswneeeeneeewneneneneseenenwnene
nenwnwwneswnwswnwnenwswnwnwsenenwnenwnwnw
wswswnwswswwswnwswwsweweswswswsw
swswenewweswwwsweewwseswwnwnw
swwwseswswwsenesweesweenwnewsese
eseeseneseewsweneeeeseeeeee
wwnwwnwnwnwwswsewnwnwnwnwewwwnw
nwneswnwswnenwenwnwnenwnwwsenwnwnwnwne
eeeeeeneneneeweeeese
seseswseswneneseseswsenwseseseswseswsesee
wswwseswswswswswewwswnwnewsww
esenwwwseswwwnewwewnewwwww
seeeeeneeseeesenweseesewnwesese
eswwewwwwnwwwwwsewwwewnwsw
seeseeseesenwnwseeswsweswseseseesesenw
eeseeweeeseeeeeeeee
seeeseewswsenwseenwseeseneneswese
nwnwnwwneneneneewnwenenenenwsenwswnw
wsesweswnewswneswsenesewswnwseswnesene
seswseseseswseseseseswnwswswseswsesenwe
newnenesewneneneeneneseneeneneneesw
swswswswwsweswswwwwswsenweswnwwne
enwwwnwswswswwsew
nwnwnwneswnenwnwnwnenenwe
nenwneneseswnenwnenwneswneswwswneseee
eneneneeneswswneneenwswenwwneseenenw
seseswnwsenwsewswnewnwwneewneswswnew
eneneenenenenenenenenewnenenenesw
neswneswwneneewseneneeewnwnenenwne
swnwnewneeseswnweneswnwsenwnwwsewe
swwenenwswwnwsweseswswsw
senenwnenenenewnwnewnwenwnwneeneswnee
wnewneswswswwneseswww
sewwweswsewseswswswswnewseeneesw
wnwnwwsewwnwneseewwwnwsenwwww
swswswwswwswewwswswswseeswswnwswsw
seswneswsewswswswseseeswsw
nwwwnwsenwwseseneewwnwnwwwwnw
swswswswnwswwswsewne
sewneswswwweseneneeswnwnwswewswswe
nenwenwenwnwwnwnenwnenenenwnwneswnenw
eeneseseseseenwseswnewsesenwwew
nenwnesenenwwnenenenwnenenwneene
swseeseseseswsenewseseeswswnwswwswnwnwne
nwnenwnenwwnewswswseenwnwnwnw
seseswseswwsweswswswsenwswsenenwswswseswsw
nwwwsewwwwwnenwnwwwwwnwsenwsew
nenwnenwnenenenwnenwnenenesewneswnenenw
neenwnenesenwnenwnwneenwseswnwwwnwne
nenwnwnesenwnwenwnwsenwnwnwnwnwnwnwnww
swwwwwswwneswwwwsewwswwswenw
nwswswwswnwseseenww
nenwsesewneeswnwnenesenesenenewnwneee
swnewweneswseswswswswswswswswneswswswnwsw
eneseneeswwnwwnwnwsesewswenwnwsewnw
nweswnenenesenwneneneneswenenenenenenwnw
swnwnweneseenwewnweeswsenwswswnwese
esenwsweenwseeseeeeeesesweeese
wwwwwwwwwwwnesewww
swseswsesesenwseswswse
seseseswseesewseswseseseswswsese
wseeswswneseseswnwnwswswswnewwwwsw
wnwenwnwswnwnwswnwewewwnwnewnwnwse
enenwnwwwnwwwsenenwswnwseneseswnenw
eeeeswseseeeeseeeenwswnweeese
nwneswswswseswswwswswseeswswswwseswswsw
swseswnwswseswneswswswswswswsweswswsww
swswswswnwswswswsenweseseswseneseswswne
eswwnweseesesewnwsesesenwseseneesesese
enwseewnesweeenenwneseeswneeswenwe
wswwswswwseewswwnwwwseswnwswenw
nwnwnwwnwnwnwswnwnwnwe
swsenwnenwnwnwnwwe
nwseeseneeeeseeeeeeeswneseswe
neeeeewnenewnesesw
nwsewesenwnwnenenwnwsewwseneenwswnwnw
nwsewwnenenenesenwnwsenwsewnesenenwse
swsenenenwseseseswswsenwweeswsenwwsesw
wewswswwswwwwnenweswwsw
sewwwwneswnewwswwwwwwwnewwww
nenwnesenwenenwnenesenenwswswenewwnenene
eneeneeseeewneeneee
seswnwwwnenwewewnwenwwnwnwsesew
swseenewnesenwwsenwwseswnese
senwwwwewsenwwnwnwnwnwswnwnenwwswnwne
nwseseseneswsenenesesenewnesesenwswwseswe
sesewnwnwnweeswnwnwwnwnwwnwnenwnwnese
nwneneneenenwneswnwnwneneneneswnenenene
wweseseswswneswsesweseswseseswsewswe
esweseeeseswnwsenwseenwsenweesew
neswseeweeesweseeseesesenwseee
nwwnwnwswesewneenwsesewwwnwnwnww
nesewneseneeneneeeeenenewnenenenenenw
eswswswwswswwwwswnwswswwwswswsene
eeneneeneenenenwnenenewneeeeewsw
swseswneswswseseswswneseneswsesesese
weweseneneesweswswnenwenwnwwseswse
nenewnweesenenwneneneneeseneeneenenese
enenwwnwnwnwnwnewnwnwwswwwnwnwwnwse
seeseswseseeenwsesenesenwseseeeese
neenwwnesenwneneneswsenenenwnwsenwnwwne
swwwswswswnwwewswnewwnewwwswswsw
seswsesesesesewsesesenewneneseesesese
neswswnwswswswseswwnesewswnwse
wseseswnwsenwswswswseseeeswnewseeswsw
seseewenwseseseesesew
seneswseswnweneseswnwwnwsenenwsenwsesww
wwseneswswwswwneseswswswswwseswswwne"""

input = input2
HexCoord = Tuple[int, int]

nw : Callable[[HexCoord], HexCoord] = lambda h: (h[0]  ,h[1]-1)
ne : Callable[[HexCoord], HexCoord] = lambda h: (h[0]+1,h[1]-1)
w  : Callable[[HexCoord], HexCoord] = lambda h: (h[0]-1,h[1]  )
e  : Callable[[HexCoord], HexCoord] = lambda h: (h[0]+1,h[1]  )
sw : Callable[[HexCoord], HexCoord] = lambda h: (h[0]-1,h[1]+1)
se : Callable[[HexCoord], HexCoord] = lambda h: (h[0]  ,h[1]+1)

flipped: set[HexCoord] = set()

_A = TypeVar('_A')
def toggle(v: _A, s: set[_A]) -> set[_A]:
    if v in s:
        s.remove(v)
    else:
        s.add(v)
    return s

for line in input.splitlines():
    current = (0,0)
    while line:
        if   line.startswith("nw"): current = nw(current); line = line[2:]
        elif line.startswith("ne"): current = ne(current); line = line[2:]
        elif line.startswith("w"):  current = w(current);  line = line[1:]
        elif line.startswith("e"):  current = e(current);  line = line[1:]
        elif line.startswith("sw"): current = sw(current); line = line[2:]
        elif line.startswith("se"): current = se(current); line = line[2:]
        else: print("wtf", line)
    flipped = toggle(current,flipped)
print(f"Day 1: {len(flipped)}")

neighbours0: list[HexCoord] = [
    ( 1, 0),
    ( 1,-1),
    ( 0,-1),
    (-1, 0),
    (-1, 1),
    ( 0, 1),
]

def neighbours(h: HexCoord) -> list[HexCoord]:
    return [(h[0]+n[0], h[1]+n[1]) for n in neighbours0]

def day(flipped: set[HexCoord]) -> set[HexCoord]:
    is_black = lambda x: x in flipped
    is_white = lambda x: x not in flipped
    black_ns = lambda x: len([n for n in neighbours(x) if is_black(n)])
    considered: set[HexCoord] = set()
    to_be_flipped: set[HexCoord] = set()

    for black in flipped:
        considered.add(black)
        ns_of_black = black_ns(black)
        if ns_of_black == 0 or ns_of_black > 2:
            to_be_flipped.add(black)

        for n in neighbours(black):
            if is_white(n) and n not in considered:
                white = n
                considered.add(white)
                ns_of_white = black_ns(white)
                if ns_of_white == 2:
                    to_be_flipped.add(white)

    for ff in to_be_flipped:
        flipped = toggle(ff, flipped)
    return flipped

def n_times(n: int, fn: Callable[[_A], _A], init: _A) -> _A:
    acc = init
    for _ in range(n):
        acc = fn(acc)
    return acc

f1 = n_times(100,day,flipped)
print(f"Day 2: {len(f1)}")
