module Year2019.Day01 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import List.Extra



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List Int


type alias Input2 =
    List Int


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> List.map Advent.unsafeToInt


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    input
        |> List.map fuelRequired
        |> List.sum


fuelRequired : Int -> Int
fuelRequired mass =
    floor (toFloat mass / 3) - 2


compute2 : Input2 -> Output2
compute2 input =
    input
        |> List.map fuelRequired2
        |> List.sum


fuelRequired2 : Int -> Int
fuelRequired2 mass =
    (List.Extra.iterate
        (\m ->
            let
                needed =
                    fuelRequired m
            in
            if needed <= 0 then
                Nothing

            else
                Just needed
        )
        mass
        |> List.sum
    )
        - mass



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example"
        "input"
        -1
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
50669
83199
108957
102490
121216
57848
76120
121042
143774
106490
76671
119551
109598
124949
148026
119862
112854
96289
73573
142714
109875
126588
69748
62766
104446
61766
133199
118306
141410
64420
129370
69444
104178
109696
54654
126517
138265
87100
130934
138946
118078
135109
137330
130913
50681
53071
63070
144616
64122
122603
86612
144666
62572
72247
79005
102223
82585
54975
68539
107964
148333
100269
134101
115960
75866
99371
56685
142199
102107
84075
112733
92180
131056
142803
145495
70900
73129
60764
77576
99160
61897
78675
100890
74787
131911
93106
91267
142663
130649
103857
81178
78896
73745
84463
92926
121715
74959
71911
89102
53428
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
