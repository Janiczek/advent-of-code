module Year2017.Day08 exposing (..)

import Advent exposing (Test)
import Dict exposing (Dict)


main : Program Never ( Output, Output ) Never
main =
    Advent.program
        { input = input
        , parse1 = parse
        , parse2 = parse
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }


type alias Input =
    List Instruction


type alias Instruction =
    { register : String
    , type_ : InstructionType
    , delta : Int
    , condRegister : String
    , condition : Condition
    , condValue : Int
    }


type InstructionType
    = Inc
    | Dec


type Condition
    = Gt
    | Gte
    | Lt
    | Lte
    | Eq
    | Neq


type alias Output =
    Maybe Int


parse : String -> Input
parse input =
    input
        |> String.lines
        |> List.map parseLine


parseLine : String -> Instruction
parseLine string =
    case String.words string of
        [ register, instruction, delta, _, condRegister, condition, condValue ] ->
            Instruction
                register
                (parseInstruction instruction)
                (Advent.toInt delta)
                condRegister
                (parseCondition condition)
                (Advent.toInt condValue)

        _ ->
            Debug.crash "weird line"


parseInstruction : String -> InstructionType
parseInstruction string =
    case string of
        "inc" ->
            Inc

        "dec" ->
            Dec

        _ ->
            Debug.crash "some new instruction"


parseCondition : String -> Condition
parseCondition string =
    case string of
        ">" ->
            Gt

        ">=" ->
            Gte

        "<" ->
            Lt

        "<=" ->
            Lte

        "==" ->
            Eq

        "!=" ->
            Neq

        _ ->
            Debug.crash "some new instruction"


compute1 : Input -> Output
compute1 input =
    let
        registers =
            Dict.empty
    in
        input
            |> List.foldl processInstruction registers
            |> Dict.values
            |> List.maximum


instructionFn : InstructionType -> (Int -> Int -> Int)
instructionFn type_ =
    case type_ of
        Inc ->
            (+)

        Dec ->
            (-)


conditionFn : Condition -> (Int -> Int -> Bool)
conditionFn condition =
    case condition of
        Gt ->
            (>)

        Gte ->
            (>=)

        Lt ->
            (<)

        Lte ->
            (<=)

        Eq ->
            (==)

        Neq ->
            (/=)


type alias Registers =
    Dict String Int


processInstruction : Instruction -> Registers -> Registers
processInstruction { register, type_, delta, condRegister, condition, condValue } registers =
    let
        condRegValue : Int
        condRegValue =
            registers
                |> get condRegister

        canProceed : Bool
        canProceed =
            conditionFn
                condition
                condRegValue
                condValue
    in
        if canProceed then
            let
                regValue : Int
                regValue =
                    registers
                        |> get register

                newValue : Int
                newValue =
                    instructionFn
                        type_
                        regValue
                        delta
            in
                registers
                    |> Dict.insert register newValue
        else
            registers


get : String -> Registers -> Int
get register registers =
    registers
        |> Dict.get register
        |> Maybe.withDefault 0


compute2 : Input -> Output
compute2 input =
    let
        registers =
            Dict.empty
    in
        input
            |> List.foldl processInstructionWithMax ( registers, 0 )
            |> Tuple.second
            |> Just


processInstructionWithMax : Instruction -> ( Registers, Int ) -> ( Registers, Int )
processInstructionWithMax { register, type_, delta, condRegister, condition, condValue } ( registers, maximum ) =
    let
        condRegValue : Int
        condRegValue =
            registers
                |> get condRegister

        canProceed : Bool
        canProceed =
            conditionFn
                condition
                condRegValue
                condValue
    in
        if canProceed then
            let
                regValue : Int
                regValue =
                    registers
                        |> get register

                newValue : Int
                newValue =
                    instructionFn
                        type_
                        regValue
                        delta
            in
                ( registers
                    |> Dict.insert register newValue
                , max newValue maximum
                )
        else
            ( registers, maximum )


tests1 : List (Test Input Output)
tests1 =
    [ Test "example"
        """b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"""
        [ Instruction "b" Inc 5 "a" Gt 1
        , Instruction "a" Inc 1 "b" Lt 5
        , Instruction "c" Dec -10 "a" Gte 1
        , Instruction "c" Inc -20 "c" Eq 10
        ]
        (Just 1)
    ]


tests2 : List (Test Input Output)
tests2 =
    [ Test "example"
        """b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"""
        [ Instruction "b" Inc 5 "a" Gt 1
        , Instruction "a" Inc 1 "b" Lt 5
        , Instruction "c" Dec -10 "a" Gte 1
        , Instruction "c" Inc -20 "c" Eq 10
        ]
        (Just 10)
    ]


input : String
input =
    """aj dec -520 if icd < 9
z dec -500 if b <= 2
zz dec 628 if z >= 499
db dec -818 if u >= 0
zmq inc -787 if fhy <= -7
icd dec 770 if z <= 502
ykm dec -317 if uol == 8
u dec 940 if u != 0
aw dec 633 if zsx <= -5
u dec -178 if qvk == 0
b inc -62 if cc <= 1
bol dec 490 if zsx < -1
cc inc -575 if aj > 525
z inc -800 if icd > -775
w inc 431 if fhy >= -6
aj dec 799 if jt >= -1
jt inc -369 if db > 809
xdp dec 681 if db <= 816
ykm inc -302 if zsx > -4
cc dec -636 if cc < 8
x dec -652 if xdp >= 6
db dec -517 if jt < -363
b dec -157 if kcr > -6
w inc 688 if y == 0
cc inc 350 if db > 1328
aj inc -458 if x == 0
y inc 661 if icd > -767
db inc 661 if kcr != -9
db inc 266 if z < -297
uol dec 227 if zmq > -6
gl inc -675 if esh != 6
uol dec -664 if esh < 6
tft dec 822 if zsx <= -10
uol inc -715 if w >= 1110
tft dec 330 if aj < -730
fhy inc -915 if w > 1117
kcr dec 275 if db < 2268
aw dec -301 if y <= 1
esh inc 825 if db > 2255
b inc -729 if zz > -638
aj dec 13 if w < 1122
icd inc 997 if uol > -282
kcr dec 836 if xdp != -8
icd dec 561 if u <= 178
qvk inc 590 if ykm >= -307
gl inc -914 if esh == 831
cc inc -5 if xdp == 0
z inc 827 if u != 185
aw inc -505 if kcr > -1119
qvk inc -443 if gl < -671
zz dec -73 if fhy != -915
u inc -721 if qvk == 147
uol dec -830 if icd == -334
ls inc 410 if icd != -344
y dec 551 if b > -642
esh dec 907 if uol > 551
xdp inc 632 if x < 5
aj inc 21 if fhy > -906
x inc 881 if aj <= -753
w dec -735 if kcr <= -1109
esh inc -807 if ls == 410
z inc -558 if u < -541
qvk dec -761 if qvk != 151
x inc -619 if uol >= 544
zsx dec -530 if db <= 2269
esh inc 806 if fhy != -909
uol inc -961 if jt >= -374
gl dec 934 if u >= -547
b dec 145 if gl > -1610
w dec 488 if ykm > -298
bol inc 803 if aw < -195
y dec -645 if b < -772
w dec 235 if xdp <= 636
x inc -641 if aj > -755
fhy dec 64 if y <= 98
qvk inc 107 if ykm < -299
zmq inc 612 if zz >= -618
tft dec 13 if esh <= -82
zz dec -306 if qvk >= 1012
db dec 528 if fhy == -979
tft inc 124 if w == 1619
kcr inc 339 if zsx > 530
kcr dec -709 if ykm >= -304
bol dec 831 if xdp >= 626
zz dec 45 if z == -31
zz dec 999 if ykm == -302
z dec 969 if uol < -406
z inc 348 if aw >= -206
db dec 388 if gl >= -1610
xdp dec -628 if aj != -743
uol inc 414 if cc == 981
aj inc 642 if uol != -4
zmq dec -163 if icd <= -334
ls dec -199 if tft >= -222
bol dec -775 if ykm != -302
y inc 43 if z < -650
z dec 239 if xdp > 1250
zz dec 961 if ykm == -302
esh inc 295 if kcr == -402
bol dec -897 if bol > -31
u inc -252 if ls != 610
bol inc -379 if xdp > 1252
b inc -521 if xdp > 1262
zmq dec -244 if z < -881
uol inc -210 if uol >= 3
esh inc 210 if zmq < 417
aj inc -888 if ykm == -302
y dec -515 if fhy < -978
zsx inc 23 if ls >= 610
jt dec -810 if fhy == -979
y inc 620 if zz != -2327
ykm dec 766 if w < 1621
jt dec -817 if db == 1346
zmq dec 237 if aj <= -993
qvk dec -587 if zz > -2330
aj dec 856 if qvk >= 1595
zz dec -316 if y != 647
uol dec 992 if kcr <= -397
jt inc 806 if esh < 424
jt dec -276 if ls == 613
b inc -154 if aj < -1851
aj inc 132 if jt != 2059
x dec 618 if jt >= 2074
tft dec 26 if uol == -1197
gl dec 505 if xdp < 1268
ykm inc 847 if fhy < -969
u inc 510 if x <= -1260
icd inc 970 if zsx <= 535
gl dec -387 if tft < -238
b dec 207 if xdp < 1266
aj inc 757 if aj > -1718
db dec 330 if qvk == 1602
qvk dec 589 if u > -290
gl dec 94 if tft <= -237
z dec 699 if kcr <= -403
bol inc -82 if xdp >= 1258
y inc -422 if qvk <= 1018
zz dec -13 if zmq < 175
xdp dec 967 if zsx <= 539
kcr inc -290 if aj > -1727
aj inc 714 if w > 1615
u inc -984 if bol != 418
ykm inc -572 if icd >= 628
icd dec 955 if jt > 2055
db inc -83 if bol < 417
w inc 882 if uol != -1190
x inc -945 if db <= 924
ls inc 207 if gl >= -1830
tft dec -178 if esh != 429
jt dec -845 if y == 230
x dec 842 if ykm >= -788
aj dec -943 if aj > -1009
db inc 785 if z == -889
zsx inc -917 if icd < -318
ls inc -757 if ls != 816
fhy dec 872 if xdp <= 294
xdp dec 937 if gl == -1821
z dec 771 if fhy != -1851
uol inc 227 if fhy >= -1852
uol dec -225 if db > 926
aj inc -60 if gl == -1821
zz inc 494 if kcr == -695
kcr inc 270 if zmq == 170
xdp inc -702 if db > 935
cc inc -158 if kcr != -413
ykm inc 351 if cc <= 832
zmq inc 784 if ls <= 818
jt inc -222 if jt <= 2910
xdp dec 495 if cc < 833
zmq dec 759 if jt > 2678
b dec -425 if w == 2501
ykm dec 280 if uol == -745
uol dec -461 if gl != -1828
jt dec -791 if jt <= 2690
qvk dec 334 if zz == -1998
aj dec 905 if tft >= -74
fhy dec -537 if y >= 221
x dec 183 if qvk != 684
u dec 524 if aw == -204
z dec 537 if xdp >= -1145
uol inc -806 if zz > -1993
u inc -54 if zmq > 186
gl inc -715 if kcr >= -423
jt inc -816 if esh >= 417
esh inc -499 if tft != -66
y inc -269 if qvk >= 670
uol inc -563 if gl < -2526
aw dec -396 if zsx > -388
jt inc 237 if kcr <= -415
u inc -700 if xdp < -1136
esh inc 109 if ls != 806
db inc -58 if b > -719
uol inc -975 if b < -711
uol inc -374 if ls != 816
db dec 24 if esh > 34
xdp inc 15 if x <= -1448
zmq inc -399 if jt >= 2900
kcr inc -778 if uol < -1823
aj inc 47 if u != -2552
gl inc -803 if aw != 184
uol inc 185 if gl == -3339
fhy inc 147 if esh >= 41
w dec 442 if zsx != -386
w dec 416 if fhy > -1306
icd dec -917 if icd > -324
jt inc -42 if esh >= 26
aw inc 184 if bol > 402
ls inc -845 if uol >= -1639
bol inc -276 if zz >= -1992
kcr inc 183 if zmq != 195
aw dec 496 if ls <= -24
w inc -358 if aw > -128
jt dec 954 if x >= -1435
xdp inc -682 if zz > -2004
qvk inc -781 if db >= 870
cc dec 883 if uol < -1627
fhy inc 313 if ykm >= -724
zmq dec -5 if db == 875
u inc 663 if db != 885
xdp dec 423 if bol < 411
aw dec -740 if bol == 408
cc inc 184 if xdp > -2248
xdp dec 129 if tft < -57
aw inc 647 if xdp <= -2366
icd inc 711 if xdp <= -2369
u dec 268 if esh < 23
b inc -506 if bol >= 402
uol inc -642 if fhy >= -1004
esh inc 97 if ykm > -732
x dec 678 if fhy != -998
w dec -304 if cc == 124
z dec 386 if gl == -3339
esh dec -686 if y <= -33
z inc -302 if u >= -1888
aw dec 39 if gl >= -3346
uol inc 24 if b == -1221
b inc 764 if z != -2116
zz inc -910 if tft >= -70
uol inc 317 if zz < -2907
z inc 5 if aj >= -983
cc dec -927 if bol != 413
bol dec 802 if jt != 2859
ls inc -308 if b != -1221
bol dec 571 if icd <= 1310
aw dec -198 if qvk >= -108
aw dec 787 if x < -2117
jt dec 329 if gl <= -3330
w dec 364 if kcr != -429
xdp dec -879 if zz >= -2915
cc dec -991 if cc == 1051
icd dec -248 if aj > -973
jt dec -39 if jt > 2525
z inc -546 if x < -2113
b dec 772 if gl > -3334
uol inc 961 if y > -33
jt dec -64 if tft > -68
w inc 366 if gl <= -3331
tft inc 455 if y == -39
u dec 387 if jt > 2638
uol inc 305 if icd >= 1301
x inc 41 if y == -39
cc inc -968 if xdp <= -1494
gl inc -579 if uol < -1629
xdp dec -290 if ls == -29
x inc -971 if z != -2667
u inc 666 if gl < -3910
fhy dec 394 if qvk > -101
y dec -927 if fhy != -997
uol inc 593 if esh >= 808
x inc 982 if x <= -3049
uol inc -38 if ykm >= -730
y inc -499 if zsx > -396
esh inc -299 if zsx == -387
bol inc 442 if uol == -1076
bol dec 93 if zsx <= -397
w dec 195 if aj > -990
jt inc -612 if uol == -1078
aj inc -391 if ykm != -727
ykm inc -222 if jt < 2011
kcr inc 138 if aj >= -1362
zmq dec -962 if w == 1812
zmq inc -769 if zsx < -396
cc dec -843 if tft <= 396
ykm inc 915 if db != 873
cc inc -684 if zsx == -387
y dec -586 if uol == -1073
cc dec 944 if y >= 388
aw dec -757 if aw == 639
gl inc 515 if zmq == 1162
fhy inc -87 if u <= -1209
icd inc 954 if ls <= -20
ls inc -267 if zz > -2916
bol inc -97 if ykm <= 202
u inc 224 if xdp >= -1212
xdp inc 306 if aw >= 1398
ls dec 86 if kcr < -417
kcr inc -482 if bol == -1062
xdp dec -461 if aw < 1398
aj dec 380 if jt != 2019
db inc -382 if zmq != 1162
aw dec -702 if qvk < -92
cc inc 445 if bol == -1062
qvk dec -339 if kcr > -912
db inc 85 if b != -1228
zsx inc -418 if ls == -382
xdp dec -46 if ls < -379
gl dec -803 if icd <= 2269
jt inc -670 if icd > 2260
z inc -515 if qvk != 237
xdp dec 630 if db <= 952
esh inc 360 if zmq != 1162
uol dec 237 if x >= -2075
uol inc 548 if db <= 967
xdp dec 868 if zmq >= 1166
y inc 970 if esh != 509
kcr dec -870 if uol >= -772
aw inc -396 if zz < -2903
ykm inc -1 if ykm != 187
aj inc -942 if y < 1363
icd inc 721 if ykm < 195
b dec 840 if zmq >= 1156
uol dec 765 if w < 1813
bol dec 436 if fhy != -1086
y inc 519 if zsx == -805
z inc 1 if icd <= 2993
zmq dec 76 if zsx <= -796
xdp dec -286 if tft > 380
cc dec -854 if xdp <= -410
esh inc 663 if zsx != -800
icd dec -575 if db > 957
cc dec 558 if db != 962
u dec 158 if zmq < 1087
xdp inc -173 if qvk == 237
db dec 366 if aw != 1697
y inc -392 if zz < -2900
b dec 657 if tft != 391
gl dec 622 if fhy <= -1095
ls dec -852 if w > 1810
aj inc -640 if u <= -1161
ykm dec 754 if w < 1819
db dec -406 if ls != 479
z dec 285 if qvk <= 235
y inc 663 if cc < 1031
fhy dec 423 if icd < 3566
x dec 499 if esh > 1177
uol inc -777 if qvk == 237
w inc -536 if z == -2656
fhy dec -834 if aj == -2314
aj inc -383 if ykm >= -564
aw inc -397 if jt >= 1343
jt inc 675 if bol == -1498
esh inc 579 if u == -1152
bol dec 428 if zz <= -2900
qvk dec 565 if icd != 3557
db dec 36 if b != -2715
w dec 334 if u > -1160
ykm inc 109 if cc >= 1034
u inc -461 if db == 964
z inc -624 if gl > -2610
w dec -264 if aj != -2691
cc inc -871 if qvk <= -322
esh dec 383 if x > -2574
jt dec -695 if zsx >= -814
aj dec 943 if esh < 1378
tft dec -604 if x != -2568
zmq dec -912 if y <= 2150
zmq inc 428 if jt < 2727
bol dec -712 if aj > -3646
aw dec -140 if ykm <= -561
cc dec 265 if tft <= 380
kcr inc -202 if fhy <= -670
qvk dec 732 if kcr < -233
ykm dec -788 if y > 2144
b dec -901 if fhy >= -683
zsx dec 378 if bol <= -1212
ls inc 311 if b >= -1824
tft dec -354 if icd < 3568
aj dec 732 if icd > 3556
zmq inc 594 if zz > -2913
w inc -255 if tft < 752
ykm dec 805 if y < 2155
ls inc -48 if ls <= 790
u dec 607 if fhy <= -683
b dec -616 if ls > 728
bol inc -406 if tft <= 746
x inc 753 if w < 945
xdp dec 831 if aw < 1438
bol dec -760 if uol < -2307
cc dec 640 if tft > 735
zsx inc 317 if b >= -1204
qvk inc 966 if u == -1613
x dec -201 if ls < 738
aj dec -358 if gl != -2594
tft inc -629 if b > -1194
qvk inc -603 if zsx <= -859
ykm inc 361 if zsx >= -870
uol dec -349 if bol < -855
z dec -770 if fhy <= -668
gl inc 707 if zsx != -864
x dec -635 if w <= 949
w dec 888 if ls >= 729
gl inc -131 if icd == 3559
tft inc 313 if esh == 1375
aj dec 383 if fhy < -674
aw inc 503 if zsx != -872
kcr dec 747 if zz <= -2912
ls dec 600 if db == 968
zsx dec 627 if u != -1612
ykm dec 83 if u <= -1615
gl dec -853 if gl <= -2016
x dec -385 if db != 968
tft dec -191 if ls != 733
w inc 931 if aj != -4397
uol inc 362 if icd != 3569
bol dec -420 if zz > -2910
b inc 788 if uol > -1599
xdp dec -603 if ls != 738
aw inc -247 if icd <= 3568
ykm inc -384 if kcr < -230
icd inc -264 if x <= -1974
b inc -673 if esh < 1375
bol inc -997 if qvk > -699
b inc -948 if uol > -1608
y inc 522 if zz != -2916
ls inc 129 if tft != 1050
gl dec -379 if w > 57
x dec -562 if icd >= 3291
b inc 83 if gl != -790
zmq inc 899 if bol != -1437
tft inc 950 if b == -1278
zz dec 689 if zz >= -2912
icd dec 107 if jt != 2719
bol inc 608 if w >= 64
xdp dec 433 if ykm >= -603
tft dec 809 if zmq > 3011
z dec 41 if u >= -1604
qvk inc 274 if x < -1416
x inc -223 if fhy <= -675
kcr dec 362 if fhy == -677
cc inc 72 if db >= 956
esh inc -197 if icd < 3302
xdp inc -258 if esh >= 1178
y inc 764 if aj > -4399
jt dec -269 if qvk >= -432
fhy inc -648 if bol < -1443
cc inc 41 if fhy <= -671
esh dec -991 if icd >= 3287
qvk inc 883 if u >= -1622
aw inc -611 if u <= -1604
ls dec -353 if zz > -3607
x dec 731 if y == 3435
esh dec 129 if aj >= -4392
gl dec 0 if y == 3435
cc dec 818 if qvk == 460
ykm dec 859 if db >= 962
uol inc -277 if fhy >= -671
zsx dec 255 if zz == -3597
db inc -431 if zz < -3589
aw inc -736 if ls < 1221
kcr dec 522 if xdp == -675
w dec -27 if icd <= 3298
tft dec 3 if uol == -1598
ls inc 809 if fhy != -678
ls dec -675 if b > -1279
jt dec 127 if x == -2364
aw dec -627 if jt > 2979
y inc -947 if zz < -3598
ykm inc -594 if kcr >= -606
bol dec -150 if ls > 2698
bol dec 162 if b <= -1275
aw inc 497 if aw <= 984
tft dec 596 if jt == 2988
zsx dec -116 if uol != -1594
bol dec 115 if bol == -1449
tft inc -222 if x >= -2380
zmq dec -334 if qvk != 469
bol dec -282 if cc > -1188
ls dec 232 if ls < 2709
x dec -984 if z >= -2518
aj inc -153 if gl != -800
gl dec 635 if aw == 1478
icd dec -503 if y < 3442
jt inc -788 if z < -2507
b inc 221 if icd == 3798
tft dec 998 if uol == -1598
aj inc 79 if xdp >= -677
esh dec 528 if qvk >= 453
w dec -591 if ykm != -2057
fhy inc -12 if cc <= -1182
ykm inc 9 if x >= -1399
qvk dec 827 if zsx != -1641
aw dec 735 if gl > -1418
zsx inc 736 if y < 3437
db inc 123 if fhy > -693
y inc 570 if tft <= -614
icd inc 689 if z < -2505
zsx dec 66 if gl != -1432
icd inc 879 if db >= 655
aw inc -423 if zsx >= -968
tft inc -896 if x != -1388
jt dec 70 if zsx <= -961
cc dec -696 if z > -2503
xdp inc -937 if jt == 2130
jt dec 670 if cc > -1188
w dec 67 if gl < -1419
qvk dec 727 if zz == -3597
esh dec -916 if w == 614
qvk inc 810 if db < 663
zmq dec -298 if fhy >= -690
y inc -276 if z > -2511
fhy inc -768 if aj >= -4476
zsx dec 825 if cc != -1185
z inc 844 if zsx < -1777
aw inc 279 if aw == 1055
qvk dec 101 if u == -1613
tft dec -134 if cc <= -1188
y dec 364 if aj == -4471
zz dec -436 if y <= 3372
u inc -367 if aw < 1336
w dec -699 if aj > -4478
z dec -759 if u != -1985
ykm inc -36 if qvk < -384
ls dec 3 if fhy != -1447
w dec -604 if w == 1313
u inc 249 if tft != -1516
uol inc -353 if gl >= -1435
jt dec -684 if qvk == -385
ls inc 481 if icd >= 5362
b dec -182 if gl >= -1429
u dec 670 if ykm > -2084
zsx inc 818 if fhy <= -1449
x dec -786 if ls == 2945
zsx dec 664 if xdp == -1609
b inc -784 if w == 1917
b dec -860 if bol <= -1291
esh dec -906 if z < -898
ykm inc 766 if aw < 1336
cc dec -81 if b != -1667
z dec 374 if y <= 3365
b dec -360 if jt == 2146
icd dec -818 if zmq < 3662
u dec -199 if bol == -1276
bol inc 179 if cc <= -1102
y inc -950 if zmq <= 3660
zmq inc -525 if u >= -2404
fhy inc -54 if zmq > 3119
z inc 366 if cc <= -1097
ls inc -232 if tft < -1517
y inc -447 if x >= -606
kcr inc -593 if xdp >= -1607
cc dec -722 if z >= -909
qvk inc 751 if uol > -1956
z dec -699 if gl <= -1428
w inc 205 if b > -1652
x inc -833 if uol > -1961
z inc -299 if aj >= -4472
w inc 985 if db < 654
uol dec 528 if zz <= -3159
w inc 286 if uol == -2479
gl inc 180 if kcr == -598
xdp inc 94 if bol <= -1098
aj inc -171 if x != -1445
aj inc -551 if aj < -4638
db dec 426 if u == -2398
w inc 591 if aw <= 1343
jt dec -806 if tft == -1519
xdp inc -392 if bol > -1109
zz dec 642 if uol >= -2481
tft dec 908 if db > 648
qvk dec 264 if jt == 2950
esh inc -650 if gl <= -1242
zsx dec 971 if bol != -1111
y inc -355 if kcr <= -598
bol dec -741 if xdp > -1915
z inc -225 if icd >= 6184
ykm inc -262 if zz != -3802
zmq inc 102 if aj <= -5188
esh dec 442 if zsx >= -2610
z inc 501 if w < 2801
uol inc 138 if icd >= 6179
x dec -203 if uol >= -2345
zz inc 3 if fhy <= -1511
zmq dec -78 if tft >= -2433
bol inc -798 if b != -1669
fhy inc -916 if jt <= 2952
cc dec -360 if uol != -2341
z inc -603 if jt >= 2944
esh dec -363 if fhy >= -2433
zmq inc 430 if uol != -2343
ykm inc -896 if u > -2404
fhy inc 265 if bol != -1168
z dec -390 if icd < 6188
zmq dec -432 if bol > -1164
ykm inc 878 if jt <= 2950
cc dec 850 if zsx < -2601
icd dec -82 if ykm < -1592
xdp inc 219 if u > -2411
aw dec 203 if xdp != -1689
y inc -351 if aw != 1133
bol dec -634 if gl > -1250
b dec 97 if cc >= -1963
aw dec -462 if zmq < 4175
fhy inc 439 if zmq > 4176
fhy dec 120 if cc <= -1954
bol dec -539 if gl <= -1245
ykm inc -165 if esh > 2730
zmq dec 420 if b <= -1750
icd dec -304 if z >= -1160
icd dec -666 if z > -1156
ykm dec -845 if fhy < -2272
bol dec -664 if jt >= 2944
u dec 998 if u != -2404
zsx dec -798 if fhy < -2277
ykm inc 318 if uol != -2332
fhy dec 49 if ls >= 2707
u inc 795 if zz >= -3806
tft dec 261 if jt == 2950
kcr dec -168 if u <= -2604
qvk dec 69 if aw > 1592
icd dec -74 if gl < -1239
x dec 564 if esh < 2736
cc dec 745 if uol != -2351
ykm dec -821 if db > 654
tft inc -778 if u == -2597
bol inc -346 if gl < -1240
qvk inc -900 if bol != 340
icd dec 603 if tft != -2698
qvk inc 189 if kcr == -430
bol inc 532 if qvk >= -680
icd inc -430 if tft >= -2696
zsx inc 105 if x == -1798
x inc 503 if esh > 2728
x inc -108 if esh > 2731
kcr inc -691 if icd >= 6273
w inc -426 if z < -1158
gl dec -636 if zsx < -1700
bol inc -787 if xdp < -1680
w dec 131 if gl == -611
uol dec -303 if gl > -613
bol dec 987 if aj == -5193
db dec 17 if aw != 1597
zz inc -52 if jt == 2950
u inc 869 if x >= -1407
jt inc 793 if kcr == -1121
gl dec -917 if b == -1752
bol inc -161 if kcr > -1131
esh dec -3 if zsx != -1698
aj inc 84 if w <= 2671
ykm inc -265 if w > 2667
zsx inc -292 if qvk == -678
icd inc 988 if qvk == -678
ls inc 727 if w == 2663
esh inc -481 if fhy == -2331
uol inc -106 if esh > 2253
fhy inc 728 if aj >= -5118
u dec -745 if y >= 1269
z inc 769 if ls == 3440
y dec -203 if tft > -2689
zmq dec 886 if fhy > -1608
ykm inc -988 if zsx <= -1994
gl dec 797 if xdp <= -1684
icd dec 186 if ykm >= 216
aw dec -840 if qvk > -676
cc dec -937 if aw == 1593
aj inc 684 if kcr != -1112
xdp inc 545 if z > -383
jt dec -521 if z == -382
qvk dec -108 if u < -1725
z dec 672 if tft == -2688
jt inc 288 if uol <= -2138
ykm inc 829 if xdp < -1137
zz inc 87 if fhy > -1604
kcr dec 354 if aw == 1593
z dec 230 if fhy != -1603
xdp inc 505 if ls < 3442
qvk inc -356 if u == -1735
uol dec -415 if fhy <= -1602
db dec -850 if z == -1054
fhy inc -597 if tft >= -2691
bol inc 764 if ykm < 1061
b dec -555 if ykm <= 1054
gl inc 665 if w >= 2662
y dec 489 if kcr < -1467
aw inc -883 if gl == -735
w dec 62 if u != -1727
db dec 672 if gl <= -739
zsx dec 905 if ykm == 1052
x dec -158 if esh < 2260
xdp dec -417 if u <= -1740
fhy inc -597 if aj >= -4431
gl inc 873 if db < 818
cc dec 176 if aw > 1587
uol inc 509 if zmq == 2863
xdp dec -463 if tft > -2691
tft inc 840 if tft <= -2684
aj inc -284 if zsx < -2897
zsx dec 940 if gl != 124
w dec 691 if zmq > 2870
x inc -371 if y > 968
ykm inc -953 if qvk == -926
cc inc -775 if u <= -1733
z dec 526 if icd < 7085
tft dec -176 if zz == -3765
b dec 601 if z > -1590
zsx dec 600 if zsx < -3837
zmq dec -655 if zmq != 2858
y inc 940 if qvk >= -933
bol inc 534 if zz != -3765
w inc -625 if b <= -1799
x inc 242 if ykm != 107
zz inc -797 if zz == -3765
aj inc -831 if esh > 2251
jt inc 593 if tft >= -1678
ls inc -634 if b < -1800
aj inc -828 if zz == -4562
ls dec 490 if u != -1735
aw dec -351 if qvk > -932
zmq inc -846 if esh != 2252
zsx dec 908 if esh == 2256
z dec -726 if z == -1580
jt dec 919 if xdp < -175
b dec -74 if ls <= 2807
jt dec 372 if esh != 2257
ls inc -704 if qvk == -928
uol inc -484 if ls != 2800
esh inc -658 if zmq >= 2680
w dec -464 if y == 1916
w inc -363 if zsx > -5352
fhy dec -960 if jt == 4773
jt inc -362 if aw <= 1951
esh inc 757 if y < 1918
u dec 534 if y != 1907
xdp inc -708 if w >= 2080
ykm inc -552 if xdp <= -173
gl inc 184 if aw > 1945
xdp inc -347 if aj == -6376
uol inc -416 if y < 1919
ykm inc 766 if kcr > -1483
b inc 890 if zsx > -5339
x dec -801 if db == 817
icd inc -735 if aw < 1948
x dec 865 if z <= -851
w dec -379 if kcr == -1475
ls dec 657 if icd < 6346
ykm dec -288 if gl <= 138
bol dec -306 if b != -1730
u dec -734 if zmq > 2670
aj dec -401 if zmq <= 2681
db inc 141 if uol > -2125
bol dec 746 if db != 953
zz dec -850 if kcr > -1478
esh inc 914 if aw >= 1939
fhy inc -420 if qvk <= -917
z dec -455 if b < -1721
aw dec 620 if b <= -1721
zmq inc 447 if esh <= 3930
ykm dec -241 if db != 958
icd dec -223 if w != 2456
jt inc -412 if gl < 132
esh inc -185 if xdp == -175
tft inc -392 if zz < -3711
x inc 610 if tft != -2064
zmq inc -261 if aw < 1333
qvk dec -606 if fhy == -2257
uol inc -717 if fhy != -2261
y dec 878 if zz >= -3719
db dec -246 if uol == -2838
db dec -331 if x > -1441
zsx inc 595 if cc == -2714
aw inc -118 if xdp != -175
y dec -828 if bol == -748
aj inc 135 if jt < 4001
esh inc -512 if fhy <= -2250
zz dec -43 if jt >= 3991
gl inc -244 if icd == 6344
zz dec -875 if aj != -5832
zz dec -182 if esh <= 3225
uol inc 98 if u > -1538
zz inc 157 if zmq < 2862
cc inc -371 if kcr == -1475
jt inc 807 if jt < 3995
u inc -76 if qvk != -313
kcr dec 134 if fhy <= -2254
db inc -724 if fhy >= -2264
icd inc -985 if kcr == -1609
gl dec -159 if aw != 1316
zmq dec -15 if b != -1718
aw inc -608 if zsx > -4745
y inc 455 if zmq == 2873
cc inc 360 if ls != 2153
zmq inc -602 if esh < 3221
gl inc -940 if ls < 2154
aj inc -922 if z >= -391
ls inc -532 if icd >= 5368
w inc 416 if zmq == 2873
jt inc 941 if jt <= 4003
zmq dec 826 if tft == -2064
zsx dec -864 if gl > -903
xdp dec 29 if esh == 3230
zsx dec -543 if tft == -2064
xdp dec -811 if jt <= 4945
y inc -557 if zz <= -3511
cc dec -134 if zsx <= -3340
fhy inc 704 if ls < 2153
gl dec 927 if uol == -2739
w dec -967 if x == -1438
gl dec -743 if zsx >= -3347
w dec 852 if db != 561
kcr inc -217 if zz != -3507
uol dec -920 if y >= 1774
ls dec 426 if w != 2987
icd dec -51 if kcr != -1819
xdp dec 173 if y != 1757
zmq inc -306 if zz != -3519
fhy dec -194 if kcr == -1826
jt inc 827 if y < 1759
b dec -391 if zsx != -3342
y dec -934 if uol > -2745
qvk inc -803 if zmq > 1733
uol dec -818 if gl >= -1084
z inc -776 if jt <= 4944
b dec -145 if zsx == -3344
zz inc 493 if aj != -5830
fhy inc 376 if fhy >= -1368
jt inc 292 if gl > -1089
u inc -58 if kcr > -1831
x inc -611 if bol == -748
y dec -509 if db == 565
zsx dec 922 if cc < -2585
zmq dec -994 if z > -1179
jt inc 3 if gl == -1079
u inc 726 if xdp != 437
u inc -771 if icd == 5415
w dec 382 if w <= 2987
b dec 474 if z == -1175
bol dec -307 if ls != 2149
qvk inc 351 if xdp != 434
fhy inc 236 if fhy >= -986
x dec -791 if jt < 5245
y inc 744 if bol != -744
ykm dec 558 if z > -1183
xdp dec -997 if kcr < -1816
gl dec 741 if b != -1666
b dec 851 if b <= -1657
u dec 962 if zz <= -3010
aw dec 266 if ls >= 2145
zz inc 157 if zmq > 2729
kcr inc -416 if cc > -2589
gl dec 174 if xdp > 1425
u dec -179 if jt > 5234
y inc 759 if icd < 5409
ykm dec -195 if jt >= 5233
b dec 528 if zmq > 2742
db inc -145 if ykm < 247
db dec -937 if fhy > -747
u dec -621 if kcr != -1835
gl inc 652 if zsx != -4271
b inc 941 if qvk == -1123
ls dec 636 if gl >= -601
b inc -571 if fhy == -747
db inc 121 if y >= 3946
zsx dec -622 if u == -1105
b inc 804 if aw != 1058
gl dec -429 if qvk == -1123
tft inc 852 if aj != -5829
esh inc -56 if tft < -1209
aj inc 287 if zz != -2862
b inc 925 if zsx >= -3648
b dec 938 if cc == -2589
fhy inc 441 if aj < -5824
y dec -326 if uol != -1914
b dec -499 if kcr >= -1828
icd inc -837 if kcr != -1829
esh dec 807 if u == -1105
icd dec 925 if w <= 2611
u dec 100 if z < -1173
jt dec 227 if esh <= 2374
b dec 531 if w >= 2612
kcr dec 730 if esh < 2371
z dec 520 if zsx < -3640
qvk dec 793 if w >= 2600
ykm dec 964 if zz >= -2870
kcr dec -977 if fhy <= -312
u inc 757 if xdp < 1441
ls inc -209 if ykm < -718
w dec -396 if jt == 5008
uol inc -480 if ykm < -720
gl dec 792 if tft >= -1220
z dec -208 if bol < -745
xdp dec 407 if zz == -2858
qvk dec 744 if kcr > -2559
db inc 355 if fhy > -313
cc inc -961 if z >= -1496
gl dec 607 if z <= -1480
uol dec 121 if zsx != -3635
qvk dec 512 if cc == -3552
bol inc 698 if zz != -2861
icd inc -729 if icd < 3657
cc inc 589 if zz > -2858
icd inc -272 if ls != 1308
w inc 20 if qvk >= -3174
kcr inc 585 if zmq != 2732
kcr inc -319 if aj > -5834
zz dec -136 if b < -721
ls inc 480 if w > 3017
bol inc -254 if esh != 2375
zmq dec 850 if zsx != -3648
xdp inc -799 if db > 899
u dec 981 if u == -455
esh dec 546 if zmq <= 1878
b dec -849 if kcr <= -2290
qvk inc -805 if ls == 1784
jt dec -180 if jt != 5008
cc dec -109 if x >= -1254
zsx inc 306 if xdp == 1431
zsx inc 763 if zmq <= 1894
aj dec 5 if cc < -3551
esh inc -506 if zmq == 1885
aw dec -199 if ls != 1789
x inc -884 if zsx >= -2568
kcr inc 132 if zmq >= 1888
bol dec -952 if zsx <= -2577
z dec -160 if aj < -5834
aj dec 432 if aw != 1251
cc dec -169 if kcr <= -2287
cc dec 312 if zsx >= -2583
b dec -335 if gl > -1564
zz dec -163 if z == -1327
ls inc -427 if b != 118
ykm dec 298 if aj < -6263
x inc -442 if gl <= -1571
ykm dec 566 if zsx != -2579
xdp inc 893 if icd != 2649
u dec -422 if qvk >= -3977
z inc -598 if bol >= -297
zz inc -794 if z >= -1324
fhy inc 131 if ykm > -1596
w inc 472 if zmq == 1895
uol dec 590 if esh != 1867
z inc -68 if w != 3018
u inc -636 if w <= 3030
icd dec 355 if tft <= -1206
tft inc 290 if x == -1700
y inc 257 if ls >= 1356
esh dec -920 if z >= -1401
x dec -607 if u > -669
xdp inc 339 if fhy >= -183
ls dec 978 if gl > -1579
x inc -159 if aw <= 1262
zz inc 104 if jt == 5008
qvk inc 606 if xdp < 2666
tft dec 668 if u > -672
gl inc 619 if cc != -3687
qvk dec 314 if y == 4534
kcr inc 963 if cc == -3695
qvk inc 38 if u >= -668
ls inc -63 if cc != -3695
z dec 916 if qvk < -3638
aj dec -649 if jt != 5012
fhy dec -227 if cc < -3688
qvk dec -846 if tft >= -1596
esh dec -886 if fhy <= 55
jt dec -444 if fhy > 42
esh inc -671 if icd != 2284
qvk dec 620 if aw < 1258
aj dec -202 if z > -2321
x dec -318 if u > -664
gl dec -295 if zz <= -2458
aj dec 878 if aj < -5409
ls inc -918 if z <= -2302
qvk inc -520 if zmq >= 1883
zsx dec -459 if db < 898
zmq inc 840 if zsx > -2122
qvk inc 351 if aj > -6299
zmq dec -362 if b == 126
u dec -833 if ls != -541
icd dec -858 if cc <= -3694
uol dec -884 if icd <= 3142
y dec -274 if cc > -3700
db inc 718 if jt <= 5458
jt dec 399 if fhy == 51
gl dec 562 if qvk <= -3593
db inc 491 if kcr > -1328
xdp inc -760 if jt == 5452
tft dec -785 if gl >= -651
aw inc -316 if zmq >= 3088
aw dec -107 if db == 2099
jt dec 377 if uol < -3102
db inc -208 if tft != -1586
zmq dec 822 if w != 3011
aj inc -119 if gl >= -660
cc inc 84 if zsx >= -2125
zmq inc 396 if ls >= -540
esh inc 553 if y > 4806
u dec -659 if kcr > -1330
db dec 455 if gl == -657
db inc -223 if esh <= 3551
ls inc 245 if w != 3020
zmq inc 495 if uol >= -3121
tft inc -704 if icd == 3150"""
