module AoC.Day01

let exampleInput: string =
    "1721
979
366
299
675
1456"

let realInput: string =
    "1962
1577
1750
1836
1762
1691
1726
1588
1370
1043
1307
1552
1813
1804
1765
1893
1610
764
1512
1404
1711
1000
1694
1546
1880
1721
2006
1787
1510
1850
1420
1712
1926
1707
1983
1680
1436
389
1448
1875
1333
1733
1935
1794
1337
1863
1769
1635
1499
1807
1326
1989
1705
1673
1829
1684
1716
456
1696
1398
1942
1851
1690
1328
1356
1775
1564
1466
1273
1896
766
1814
1810
1537
1463
1755
1341
1665
1520
1366
1387
1976
1717
1737
1551
1760
1496
1664
1450
1319
1674
1630
1301
1330
1658
1637
1655
1439
1832
1948
1339
1656
1449
1296
1489
1758
1939
1857
1402
1394
1882
1446
1412
1430
1212
1377
1501
1873
1812
1667
1560
1654
1575
1999
1581
1792
1299
1843
1383
1351
1297
1822
1801
1977
1316
1477
1980
1693
1220
1554
1607
1903
1669
1593
1955
1286
1909
1280
1854
2005
1820
1803
1763
1660
1410
1974
1808
1816
1723
1936
1423
1818
1800
1294
857
496
1248
1670
1993
1929
1966
1381
1259
1285
1797
1644
1919
1267
1509
399
1300
1662
1556
1747
1517
1972
1729
1506
1544
1957
1930
1956
1753
1284
1389
1689
1709
1627
1770
847"

let parse1 (input: string): int list =
    input.Split "\n"
    |> seq
    |> Seq.map int
    |> Seq.toList

let isOk (x: int) (y: int): bool = x + y = 2020

// x always before y in the list... we try all ys for one x, then go for next x
// a b c d e f g
// ^ ^
// ^   ^
// ^     ^
// ^       ^
// ...
//   ^ ^
//   ^   ^
let rec go1 (x: int) (xs: int list): int =
    match xs with
    | [] -> failwith "didn't find anything"
    | y :: ys ->
        match checkAllForX x xs with
        | None -> go1 y ys
        | Some ((a, b)) -> a * b

and checkAllForX (x: int) (xs: int list): (int * int) option =
    match xs with
    | [] -> None
    | y :: ys -> if isOk x y then Some(x, y) else checkAllForX x ys

let compute1 (input: int list): int =
    match input with
    | [] -> failwith "empty list?"
    | x :: xs -> go1 x xs

let compute2 (input: int list): int option list =
    [ for x in input do
        for y in input do
            for z in input ->
                if x <> y && y <> z && x + y + z = 2020 then
                    Some(x * y * z)
                else
                    None ]
    |> List.filter (fun thing -> thing.IsSome)

let main = realInput |> parse1 |> compute2
