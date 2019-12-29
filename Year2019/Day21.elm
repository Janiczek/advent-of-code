module Year2019.Day21 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import Dict exposing (Dict)
import Fifo
import List.Extra
import Set exposing (Set)
import Year2019.Intcode as Intcode
    exposing
        ( Computer
        , OutputError(..)
        , Stop(..)
        )
import Year2019.Intcode.Memory as Memory exposing (Memory)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    Memory


type alias Input2 =
    Memory


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    Memory.fromString string
        |> Advent.unsafeMaybe "parse1"


parse2 : String -> Input2
parse2 string =
    Memory.fromString string
        |> Advent.unsafeMaybe "parse2"



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 mem =
    mem
        |> Intcode.initWithMemory
        |> Intcode.stepUntilStopped
        |> Intcode.addAsciiInput_ """NOT A T
NOT T T
AND B T
AND C T
NOT T J
AND D J
WALK
"""
        |> Intcode.stepUntilStopped_
        |> Intcode.getOutputs
        |> Tuple.first
        |> List.reverse
        |> List.head
        |> Advent.unsafeMaybe "compute1"


compute2 : Input2 -> Output2
compute2 mem =
    mem
        |> Intcode.initWithMemory
        |> Intcode.stepUntilStopped
        |> Intcode.addAsciiInput_ """NOT A T
NOT T T
AND B T
AND C T
NOT T J
AND D J
NOT H T
NOT T T
OR E T
AND T J
RUN
"""
        |> Intcode.stepUntilStopped_
        |> Intcode.getOutputs
        |> Tuple.first
        |> List.reverse
        |> List.head
        |> Advent.unsafeMaybe "compute2"



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example"
        "input"
        Nothing -- Just "parsed-input"
        -1
     -}
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
109,2050,21102,1,966,1,21101,13,0,0,1106,0,1378,21101,20,0,0,1106,0,1337,21102,1,27,0,1105,1,1279,1208,1,65,748,1005,748,73,1208,1,79,748,1005,748,110,1208,1,78,748,1005,748,132,1208,1,87,748,1005,748,169,1208,1,82,748,1005,748,239,21102,1041,1,1,21101,0,73,0,1105,1,1421,21102,1,78,1,21101,0,1041,2,21101,0,88,0,1105,1,1301,21101,68,0,1,21102,1,1041,2,21102,1,103,0,1106,0,1301,1101,1,0,750,1106,0,298,21102,82,1,1,21101,0,1041,2,21101,125,0,0,1106,0,1301,1102,2,1,750,1106,0,298,21102,1,79,1,21101,1041,0,2,21101,0,147,0,1105,1,1301,21101,84,0,1,21102,1,1041,2,21101,0,162,0,1105,1,1301,1101,0,3,750,1106,0,298,21101,65,0,1,21101,1041,0,2,21101,184,0,0,1105,1,1301,21102,76,1,1,21101,1041,0,2,21101,199,0,0,1105,1,1301,21101,75,0,1,21101,0,1041,2,21102,1,214,0,1106,0,1301,21101,0,221,0,1105,1,1337,21102,1,10,1,21101,1041,0,2,21101,0,236,0,1105,1,1301,1105,1,553,21102,85,1,1,21101,1041,0,2,21101,254,0,0,1105,1,1301,21101,0,78,1,21101,0,1041,2,21101,0,269,0,1105,1,1301,21102,1,276,0,1106,0,1337,21101,0,10,1,21101,0,1041,2,21102,291,1,0,1106,0,1301,1102,1,1,755,1105,1,553,21102,1,32,1,21101,1041,0,2,21101,313,0,0,1105,1,1301,21101,0,320,0,1106,0,1337,21101,0,327,0,1105,1,1279,1201,1,0,749,21102,1,65,2,21101,0,73,3,21102,1,346,0,1106,0,1889,1206,1,367,1007,749,69,748,1005,748,360,1101,0,1,756,1001,749,-64,751,1106,0,406,1008,749,74,748,1006,748,381,1101,0,-1,751,1105,1,406,1008,749,84,748,1006,748,395,1102,1,-2,751,1105,1,406,21102,1100,1,1,21101,406,0,0,1105,1,1421,21102,32,1,1,21101,1100,0,2,21101,0,421,0,1105,1,1301,21101,0,428,0,1106,0,1337,21101,0,435,0,1106,0,1279,1202,1,1,749,1008,749,74,748,1006,748,453,1101,-1,0,752,1105,1,478,1008,749,84,748,1006,748,467,1101,0,-2,752,1105,1,478,21102,1,1168,1,21101,0,478,0,1105,1,1421,21101,485,0,0,1105,1,1337,21101,0,10,1,21102,1168,1,2,21102,500,1,0,1106,0,1301,1007,920,15,748,1005,748,518,21102,1209,1,1,21101,0,518,0,1105,1,1421,1002,920,3,529,1001,529,921,529,102,1,750,0,1001,529,1,537,102,1,751,0,1001,537,1,545,1001,752,0,0,1001,920,1,920,1105,1,13,1005,755,577,1006,756,570,21102,1,1100,1,21101,570,0,0,1105,1,1421,21101,987,0,1,1105,1,581,21102,1001,1,1,21102,1,588,0,1105,1,1378,1101,758,0,594,102,1,0,753,1006,753,654,20101,0,753,1,21101,610,0,0,1106,0,667,21102,0,1,1,21102,621,1,0,1105,1,1463,1205,1,647,21101,0,1015,1,21102,1,635,0,1106,0,1378,21101,0,1,1,21101,646,0,0,1106,0,1463,99,1001,594,1,594,1106,0,592,1006,755,664,1101,0,0,755,1105,1,647,4,754,99,109,2,1101,726,0,757,22102,1,-1,1,21101,0,9,2,21102,697,1,3,21101,0,692,0,1105,1,1913,109,-2,2105,1,0,109,2,102,1,757,706,2101,0,-1,0,1001,757,1,757,109,-2,2105,1,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,255,63,159,95,127,191,223,0,126,231,38,109,170,79,250,47,156,49,184,243,242,162,216,111,222,172,114,68,51,185,61,244,197,35,179,205,199,247,56,125,118,252,237,227,117,94,102,221,178,251,173,248,203,186,187,103,238,214,201,120,70,217,202,142,69,212,99,174,39,60,139,58,233,163,157,245,253,46,62,167,55,42,53,123,232,43,155,98,169,207,228,158,107,59,113,196,106,92,198,34,204,115,206,84,93,108,181,76,153,189,230,100,78,110,119,137,71,235,136,188,86,218,254,141,138,239,219,77,190,166,220,226,249,213,241,54,124,177,122,116,87,183,234,143,101,168,246,154,229,175,200,57,236,85,215,140,50,152,182,121,171,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20,73,110,112,117,116,32,105,110,115,116,114,117,99,116,105,111,110,115,58,10,13,10,87,97,108,107,105,110,103,46,46,46,10,10,13,10,82,117,110,110,105,110,103,46,46,46,10,10,25,10,68,105,100,110,39,116,32,109,97,107,101,32,105,116,32,97,99,114,111,115,115,58,10,10,58,73,110,118,97,108,105,100,32,111,112,101,114,97,116,105,111,110,59,32,101,120,112,101,99,116,101,100,32,115,111,109,101,116,104,105,110,103,32,108,105,107,101,32,65,78,68,44,32,79,82,44,32,111,114,32,78,79,84,67,73,110,118,97,108,105,100,32,102,105,114,115,116,32,97,114,103,117,109,101,110,116,59,32,101,120,112,101,99,116,101,100,32,115,111,109,101,116,104,105,110,103,32,108,105,107,101,32,65,44,32,66,44,32,67,44,32,68,44,32,74,44,32,111,114,32,84,40,73,110,118,97,108,105,100,32,115,101,99,111,110,100,32,97,114,103,117,109,101,110,116,59,32,101,120,112,101,99,116,101,100,32,74,32,111,114,32,84,52,79,117,116,32,111,102,32,109,101,109,111,114,121,59,32,97,116,32,109,111,115,116,32,49,53,32,105,110,115,116,114,117,99,116,105,111,110,115,32,99,97,110,32,98,101,32,115,116,111,114,101,100,0,109,1,1005,1262,1270,3,1262,20101,0,1262,0,109,-1,2105,1,0,109,1,21102,1288,1,0,1105,1,1263,20101,0,1262,0,1101,0,0,1262,109,-1,2106,0,0,109,5,21101,0,1310,0,1105,1,1279,21201,1,0,-2,22208,-2,-4,-1,1205,-1,1332,21201,-3,0,1,21102,1332,1,0,1105,1,1421,109,-5,2105,1,0,109,2,21102,1,1346,0,1106,0,1263,21208,1,32,-1,1205,-1,1363,21208,1,9,-1,1205,-1,1363,1106,0,1373,21101,0,1370,0,1106,0,1279,1106,0,1339,109,-2,2105,1,0,109,5,2101,0,-4,1385,21002,0,1,-2,22101,1,-4,-4,21102,0,1,-3,22208,-3,-2,-1,1205,-1,1416,2201,-4,-3,1408,4,0,21201,-3,1,-3,1105,1,1396,109,-5,2105,1,0,109,2,104,10,22101,0,-1,1,21101,1436,0,0,1106,0,1378,104,10,99,109,-2,2105,1,0,109,3,20002,594,753,-1,22202,-1,-2,-1,201,-1,754,754,109,-3,2105,1,0,109,10,21102,5,1,-5,21101,1,0,-4,21102,1,0,-3,1206,-9,1555,21102,1,3,-6,21101,0,5,-7,22208,-7,-5,-8,1206,-8,1507,22208,-6,-4,-8,1206,-8,1507,104,64,1106,0,1529,1205,-6,1527,1201,-7,716,1515,21002,0,-11,-8,21201,-8,46,-8,204,-8,1105,1,1529,104,46,21201,-7,1,-7,21207,-7,22,-8,1205,-8,1488,104,10,21201,-6,-1,-6,21207,-6,0,-8,1206,-8,1484,104,10,21207,-4,1,-8,1206,-8,1569,21102,1,0,-9,1105,1,1689,21208,-5,21,-8,1206,-8,1583,21102,1,1,-9,1106,0,1689,1201,-5,716,1589,20101,0,0,-2,21208,-4,1,-1,22202,-2,-1,-1,1205,-2,1613,22101,0,-5,1,21101,1613,0,0,1105,1,1444,1206,-1,1634,21201,-5,0,1,21101,1627,0,0,1106,0,1694,1206,1,1634,21102,2,1,-3,22107,1,-4,-8,22201,-1,-8,-8,1206,-8,1649,21201,-5,1,-5,1206,-3,1663,21201,-3,-1,-3,21201,-4,1,-4,1105,1,1667,21201,-4,-1,-4,21208,-4,0,-1,1201,-5,716,1676,22002,0,-1,-1,1206,-1,1686,21101,0,1,-4,1105,1,1477,109,-10,2105,1,0,109,11,21101,0,0,-6,21102,0,1,-8,21101,0,0,-7,20208,-6,920,-9,1205,-9,1880,21202,-6,3,-9,1201,-9,921,1724,21002,0,1,-5,1001,1724,1,1733,20101,0,0,-4,22101,0,-4,1,21101,0,1,2,21101,9,0,3,21101,0,1754,0,1106,0,1889,1206,1,1772,2201,-10,-4,1766,1001,1766,716,1766,21002,0,1,-3,1105,1,1790,21208,-4,-1,-9,1206,-9,1786,22102,1,-8,-3,1105,1,1790,22101,0,-7,-3,1001,1733,1,1795,21001,0,0,-2,21208,-2,-1,-9,1206,-9,1812,21202,-8,1,-1,1106,0,1816,22102,1,-7,-1,21208,-5,1,-9,1205,-9,1837,21208,-5,2,-9,1205,-9,1844,21208,-3,0,-1,1105,1,1855,22202,-3,-1,-1,1105,1,1855,22201,-3,-1,-1,22107,0,-1,-1,1106,0,1855,21208,-2,-1,-9,1206,-9,1869,21202,-1,1,-8,1105,1,1873,21202,-1,1,-7,21201,-6,1,-6,1106,0,1708,22101,0,-8,-10,109,-11,2106,0,0,109,7,22207,-6,-5,-3,22207,-4,-6,-2,22201,-3,-2,-1,21208,-1,0,-6,109,-7,2105,1,0,0,109,5,1201,-2,0,1912,21207,-4,0,-1,1206,-1,1930,21102,0,1,-4,21202,-4,1,1,22101,0,-3,2,21102,1,1,3,21102,1949,1,0,1106,0,1954,109,-5,2106,0,0,109,6,21207,-4,1,-1,1206,-1,1977,22207,-5,-3,-1,1206,-1,1977,22102,1,-5,-5,1105,1,2045,22101,0,-5,1,21201,-4,-1,2,21202,-3,2,3,21102,1,1996,0,1106,0,1954,21201,1,0,-5,21101,0,1,-2,22207,-5,-3,-1,1206,-1,2015,21102,1,0,-2,22202,-3,-2,-3,22107,0,-4,-1,1206,-1,2037,22101,0,-2,1,21102,1,2037,0,106,0,1912,21202,-3,-1,-3,22201,-5,-3,-5,109,-6,2105,1,0
"""
        |> Advent.removeNewlinesAtEnds


main : Program () ( Output1, Output2 ) Never
main =
    Advent.program
        { input = input_
        , parse1 = parse1
        , parse2 = parse2
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }
