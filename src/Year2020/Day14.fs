module AoC.Day14

open FSharpPlus

let exampleInput = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"

let exampleInput2 = "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"

let realInput = "mask = 100110001110110011001X101110X1XX10X1
mem[62998] = 9708340
mem[33440] = 124867031
mem[6869] = 81126
mask = 11110000X01010010XX10X000XX00X001010
mem[13476] = 173113
mem[39561] = 27229922
mem[35430] = 347364280
mem[59598] = 3650
mem[13531] = 844841709
mask = X0110X00011X1110X11010000010110001X1
mem[39480] = 271401
mem[50956] = 752277993
mem[368] = 2231
mem[17908] = 376977
mask = 011110110XX0110001XX0X01XX0010100100
mem[56735] = 1733781
mem[49770] = 7833115
mask = 100110001X101100XX1010001000011011X0
mem[35668] = 9080392
mem[2118] = 93303
mem[22017] = 10163
mem[55863] = 549388
mem[40152] = 51663168
mem[46928] = 118804934
mem[40585] = 4540
mask = X1X1101X0X101X0001100X010X101XX01010
mem[8325] = 251174
mem[38881] = 71910194
mem[56331] = 3712330
mem[15825] = 244573
mem[55311] = 226546976
mem[53810] = 742
mask = 1001100010101000X0XX10001001100X0001
mem[49554] = 32033936
mem[44420] = 2749
mem[8451] = 461613
mem[23507] = 4532710
mask = 1X011X0X10X00X001010X011101100000011
mem[60294] = 31518
mem[29569] = 6578686
mem[21008] = 1987459
mem[1462] = 36088
mem[37118] = 3859341
mem[10082] = 173198
mask = X1X11X00X01010XX011X1001100010101010
mem[26828] = 15660795
mem[42418] = 2990
mem[44273] = 217
mem[10595] = 3430553
mask = 10X110X010101X001010101X100X00X00XX1
mem[10082] = 588571
mem[52784] = 16169
mem[49920] = 132826443
mask = 10011010X11XX000111001000X0000100111
mem[51727] = 69340093
mem[27169] = 237758969
mem[32259] = 253
mask = 011110110010XX0001X0X1110XX0X0X0110X
mem[36935] = 26074
mem[1600] = 26798141
mem[54701] = 104718973
mem[1202] = 65187
mem[33150] = 1530
mem[18022] = 2879928
mem[50234] = 69908
mask = 1X1110X0X1101X00X11X110X10100XX00111
mem[17718] = 60893480
mem[27610] = 269
mem[48550] = 47978091
mem[27654] = 589
mem[7817] = 3506071
mem[61282] = 480898
mem[9821] = 21368
mask = X110X0001X1011X0111X11111010XX001001
mem[33947] = 100432
mem[54689] = 10785
mem[17652] = 22244234
mem[29826] = 1532
mem[56926] = 3461025
mem[7643] = 3081
mem[44251] = 18658718
mask = 1XX110001110X1001X10111000100XXXX011
mem[52316] = 1056
mem[25096] = 13948
mem[29569] = 95944410
mask = 111110101X101XXX1110X10X101X10X101X1
mem[198] = 56909312
mem[19535] = 134
mem[7126] = 42965
mem[24971] = 825
mask = X1111000011XX000011101100X100011XX00
mem[44547] = 2611
mem[39021] = 738
mem[37713] = 456837
mask = 100100X00010X100101XX000111011110X0X
mem[55643] = 17218
mem[11517] = 11285
mem[57130] = 16487599
mem[41286] = 31096
mem[49360] = 1021109
mask = 0111100100100XX0001000000X10XX0011X1
mem[2374] = 76911
mem[64064] = 18297184
mem[59096] = 68536630
mem[21158] = 86340
mem[53009] = 49
mem[32603] = 151826
mask = 1X0110001010X0001X10100010010X0X0001
mem[23391] = 444627
mem[41426] = 48722
mem[57856] = 134238192
mem[17718] = 43897845
mem[62019] = 14442
mem[42445] = 1647
mask = 101X10100X10110X10XX011X0000X00X0000
mem[10928] = 2500
mem[20838] = 56999423
mem[25999] = 184909525
mem[26007] = 2123012
mem[48938] = 45692507
mask = 10X1101011101000111X1001X10X10000111
mem[50241] = 78378235
mem[44902] = 61326
mem[49459] = 529618451
mem[9858] = 1900346
mem[53729] = 323
mem[36784] = 18539
mask = 1X1100100X1X10101X1000X10010001X1100
mem[33440] = 714357
mem[8780] = 9457
mem[39405] = 94677301
mask = 1X1X10001X1011X011111110111000011X01
mem[7695] = 76593
mem[29551] = 8037042
mem[38881] = 395452049
mem[18026] = 1315
mem[63110] = 896
mem[32635] = 2312
mem[51980] = 5007732
mask = 01XX100101100000X010001011100001X001
mem[2310] = 20405
mem[53937] = 363909044
mem[51663] = 90768255
mask = 111100XX1110100X0110101XX0X00X10X111
mem[1690] = 13462
mem[52167] = 25867535
mem[44142] = 3236766
mem[61314] = 96933150
mem[60738] = 805417
mem[54375] = 517
mem[22340] = 2896
mask = 111110X011101000X1111X00010XXX011111
mem[59683] = 332943
mem[63316] = 31352372
mem[20057] = 36814138
mem[54353] = 1914411
mem[34386] = 6370
mem[59914] = 47956
mem[6408] = 59925239
mask = 1011X0000X10100010X00X1011X00X100011
mem[6521] = 480923757
mem[1462] = 264154162
mem[61394] = 809
mask = 11111010011XX00001100X101011X010010X
mem[9835] = 1799371
mem[32239] = 127302446
mem[51663] = 544
mem[57856] = 66667817
mem[2491] = 127
mem[8333] = 2276312
mem[58915] = 22092
mask = 111100X01X1XX001X110111101000X100010
mem[64589] = 4258915
mem[47487] = 285752
mem[6869] = 1596
mask = 1111X0X0X010110011101101000110X1X001
mem[5453] = 91496217
mem[2646] = 2667
mem[21050] = 353326983
mask = X01110100X101X001010X0X1X01001100001
mem[45101] = 70813816
mem[44546] = 308
mem[57631] = 9039857
mem[27169] = 3160
mask = 0110001X1110X1001100001000X01110X010
mem[28718] = 19694
mem[42115] = 59579390
mem[40413] = 43201
mem[59683] = 199166
mask = 10110010111011000X1XX11101XXX00X0011
mem[19784] = 26996
mem[9835] = 26757939
mem[1355] = 639095
mem[22997] = 3076780
mem[18929] = 1000155
mask = 10011000X1X00XX011X001000XX00X000011
mem[41473] = 15683
mem[10654] = 300147
mem[65299] = 5404
mem[49446] = 262001143
mem[48896] = 77288
mask = 10111010X110X10011110101X0100000X11X
mem[14093] = 65927877
mem[48550] = 65429
mem[40585] = 125589
mem[59196] = 241585409
mask = 0111001X10100001X10001000X101100X111
mem[19535] = 97672040
mem[34960] = 223021623
mem[11445] = 12973820
mem[31234] = 111455
mem[17045] = 6307012
mem[35956] = 12190194
mem[23822] = 198622
mask = 010X11001110X0110110100X0100000X0110
mem[17729] = 128199106
mem[6566] = 991727748
mem[40620] = 518974
mem[62157] = 7223
mem[51624] = 2746037
mask = X1011010X11XX000X110X10X10110100001X
mem[12314] = 7438
mem[30174] = 861079
mem[61319] = 237771
mem[56184] = 550369639
mem[24866] = 11544
mask = 1111100011X01100011XX1100000000001X1
mem[25096] = 11465
mem[41174] = 19609
mem[61765] = 9254
mem[44546] = 18901638
mask = 0X011011011011X0011010X10000100X1010
mem[21144] = 125649840
mem[37854] = 461670
mem[36768] = 1823059
mem[21797] = 106819
mask = 101110X0XX1011001010X1XX101X001X0000
mem[881] = 5602
mem[24867] = 2323430
mem[1375] = 1042520
mem[914] = 8283690
mem[7056] = 70100608
mem[17908] = 2904425
mask = 010110111X100X010X00011001110111X1XX
mem[287] = 5052060
mem[64589] = 19158826
mem[41925] = 47066119
mem[44818] = 57717343
mask = 10X1X01001101010X110001X1X0110110110
mem[45178] = 183479
mem[39426] = 4005802
mem[64828] = 5710008
mask = 11XX111010001X00101000111111X1000000
mem[48814] = 28141
mem[30828] = 7849
mem[48068] = 249
mem[34905] = 4062
mask = 10X100X01X101X0000110X1XX00011010X1X
mem[5461] = 408004
mem[32538] = 15303536
mem[59914] = 128160
mem[23777] = 5181173
mask = 1X11X0001110100X011X1X11X1X00010X11X
mem[60828] = 99826852
mem[62051] = 26656040
mem[55017] = 166822210
mask = X11XX01100100X000X00X01001X10101X1X0
mem[30302] = 2574
mem[8333] = 29343
mem[61663] = 439018
mem[13806] = 456141
mask = 0X100011011X0X00001X1100XXX0110X1101
mem[22292] = 125832761
mem[31398] = 221057
mem[16344] = 423
mem[19616] = 483547
mem[57645] = 2723
mem[62051] = 9731067
mask = 111X1000X11110X00X1X10010X1001011101
mem[24876] = 3544242
mem[6961] = 595
mem[63316] = 59604424
mem[3938] = 2322
mask = 0111X0X10X100X000X10011X0100X010X0X1
mem[54349] = 176922
mem[44939] = 1720
mem[56849] = 65044820
mask = 01X0001X011000X000X1X100011011110X00
mem[26038] = 79135
mem[28760] = 48844339
mem[5425] = 65454644
mem[2081] = 7433
mem[1139] = 98313
mask = X11X1X0011101001X110101101XX00X111X0
mem[36935] = 685782169
mem[48050] = 637647920
mem[36526] = 467954
mask = 00011XX0101X1001011000010X1X1X100XX0
mem[12545] = 23720
mem[48741] = 831
mem[54810] = 861
mem[35882] = 11960
mem[65049] = 6566
mem[31876] = 47205
mem[42306] = 2957
mask = 1X0100001010X1000011011X11XX10010010
mem[29794] = 3311
mem[7279] = 17678061
mem[35574] = 57
mem[34279] = 14957254
mem[53099] = 578302
mem[23935] = 101040071
mem[48741] = 557
mask = X01X0010011011X010100X110X010000X110
mem[36768] = 24451
mem[5966] = 1399
mem[29703] = 368831
mem[17057] = 3756
mem[41426] = 1666
mask = 1XX1X0X01110110011X01101X010X1001100
mem[40493] = 377542989
mem[26606] = 1340407
mem[5425] = 340433144
mem[19975] = 99977737
mem[9463] = 240
mem[49767] = 6599426
mask = 1111X0001X1010010111XXX1X0000110X110
mem[52003] = 475150
mem[55311] = 904259
mem[33150] = 16080
mem[61664] = 4435
mem[13992] = 975381
mem[62465] = 12445079
mask = 10111XX011X0110010100XXX10101X110000
mem[17650] = 340568
mem[47124] = 183
mem[63115] = 30913513
mem[60628] = 138264062
mask = 11111X0011101X000X10100110X000X0X1X0
mem[24971] = 31365
mem[44951] = 11728619
mem[25696] = 302033
mem[21301] = 9627
mem[44157] = 525244205
mem[50656] = 10378362
mask = X1X11010X110XX0011X011001X1100000111
mem[21359] = 33302
mem[32262] = 1195
mem[36724] = 70173555
mask = 011110X0001010000X10110X00X00X0X00X0
mem[34040] = 76148
mem[27225] = 17171
mem[5435] = 3821
mem[40522] = 986
mem[29533] = 5879
mem[58995] = 468861340
mask = 1X111010000011001010X1100011101X11X0
mem[4300] = 1968578
mem[60134] = 113072
mem[52745] = 214174
mem[21584] = 931098775
mem[35748] = 183615
mem[5425] = 310
mem[44273] = 25744270
mask = 0110101100X000000100X11101X001X10110
mem[22539] = 628206922
mem[21144] = 264564
mem[62744] = 2994725
mask = 011X00101110110X1X000111X01001111001
mem[22340] = 15133495
mem[12926] = 1765
mem[35190] = 4163491
mask = 01X111001110X0X001100100XX01X000X1X1
mem[40522] = 59810
mem[56396] = 517415
mem[524] = 255482833
mem[26606] = 981177164
mask = 1011100001101100111XXX010011011101X1
mem[59683] = 9400311
mem[20939] = 595722054
mem[63721] = 6985592
mem[57138] = 1269
mem[15281] = 5401
mask = 01XXX100111010XXX1100X00101X1000011X
mem[2313] = 688104
mem[32541] = 269387
mem[56619] = 791767610
mem[46108] = 26686613
mem[50343] = 355889
mask = 11X10X0X11X0X0010110XX01100X0110X011
mem[63972] = 61249
mem[62051] = 33442568
mem[2467] = 34752053
mem[60551] = 484021
mask = 1001X010X110100X11100011XXX000X0010X
mem[47266] = 2157244
mem[44919] = 157513051
mem[32262] = 1218209
mem[58259] = 10042236
mask = 111XX0001X10XX00011000100X0001X01110
mem[38117] = 190943177
mem[53165] = 111633760
mem[13992] = 57901936
mem[34729] = 10392
mask = 01X1XX11X010000X010001X00000X1XX1110
mem[53210] = 649683386
mem[1139] = 32691865
mem[30132] = 7760
mem[54114] = 389230
mem[25317] = 12494
mem[2009] = 16139248
mem[13666] = 22025
mask = 0001X1001X10100001X0000X1111X0100011
mem[35936] = 3218400
mem[32130] = 6979
mem[3463] = 975563095
mem[26275] = 666625528
mem[2491] = 1009324251
mask = X11110X1011000000X10111X1X00000X1X01
mem[37893] = 77491794
mem[62284] = 2622180
mem[54953] = 608
mem[13806] = 3053695
mem[851] = 91893573
mem[44236] = 18748041
mask = X1111000111010010X11X1X101X00110X110
mem[6408] = 77
mem[17173] = 105746610
mem[61958] = 3970453
mem[11809] = 530128754
mem[50353] = 397
mask = 01X0X011011101XXX01X1111X01011100110
mem[56977] = 37625
mem[7087] = 17032
mem[8365] = 771433
mem[18057] = 3004
mem[50234] = 186750457
mem[18676] = 707230
mask = 01X11X1001101100011X0X0100X00110X001
mem[57493] = 32401
mem[8878] = 51376
mem[22997] = 186778624
mem[47670] = 30514
mem[17652] = 630094
mem[3020] = 1035621818
mem[16186] = 35187162
mask = X110100011X0100X01100X10011011X0111X
mem[15353] = 475655169
mem[42306] = 737698
mem[46697] = 986478
mem[57579] = 1677835
mem[38238] = 24794941
mask = 101X1010111011001111111X0X0X0001X110
mem[57748] = 261680967
mem[32426] = 380599
mem[27169] = 3161369
mem[15975] = 30083679
mask = 11X11010X010101111101001111X011X1100
mem[57551] = 17499196
mem[64471] = 83949191
mem[22431] = 127776708
mem[56954] = 7791803
mem[52316] = 12457590
mem[23210] = 100901
mask = 1011101X011X10X0101010000XX1001X110X
mem[6336] = 6091946
mem[3938] = 1103
mem[48806] = 6054
mask = X00100000010110010100X0111XX0011XX10
mem[46212] = 868976
mem[60751] = 38686
mem[7817] = 24028835
mem[26026] = 1375636
mem[27924] = 99908083
mem[39820] = 1819
mask = 011110X10X10XX000X10X1X1000X0010110X
mem[61701] = 52009386
mem[21486] = 120670
mem[22778] = 3607
mask = 1X111X101XX011001010000110XX00XXX000
mem[48791] = 2494667
mem[21251] = 22054181
mem[33066] = 43763
mem[17993] = 1042
mem[44236] = 14520
mem[34723] = 229954479
mask = 1011001X1110X10000X11XXX0101X00X0011
mem[37118] = 2636
mem[25666] = 3141063
mem[1136] = 51218577
mem[9755] = 3174
mem[28778] = 39021
mask = 1X11X01011101100X11X0X10X1X100100X11
mem[6900] = 10217
mem[46260] = 5912342
mem[44033] = 23158
mem[60828] = 18854
mask = X1101X00111011X011X101XX11101X101000
mem[534] = 59485
mem[50433] = 6359279
mem[32990] = 13853
mem[25149] = 7619
mem[35997] = 53053
mem[17057] = 340332368
mask = 10X11X1000XX1X0010X00100110000X11101
mem[59073] = 79091023
mem[60294] = 236544
mem[49767] = 42279458
mask = 0111X10X111X10X011X010101XX00X0111X0
mem[63115] = 7199
mem[16007] = 64293
mem[15080] = 508680
mem[59082] = 12677
mem[38645] = 55217038
mem[21795] = 892
mem[31087] = 31722
mask = 111X100011X01001X0111X110000X101X111
mem[2770] = 193639
mem[35074] = 418456
mem[12634] = 496715746
mem[17650] = 1005
mask = 0111X00101XX00X0X0100011010000X00XX0
mem[54341] = 70895761
mem[5883] = 22610
mask = 01XX001101100000001X11X00XX0111X010X
mem[9219] = 49968
mem[62160] = 112464674
mem[2770] = 2139
mem[52316] = 6520
mem[10444] = 220757
mem[11517] = 107554282
mem[62689] = 224389
mask = 1101100010100X001110X1101X1100X00001
mem[11503] = 241583
mem[43476] = 6546854
mem[27201] = 12413
mask = 01100X1X011X00X00011010X0X0011X00001
mem[54223] = 1042
mem[15018] = 5591
mem[34905] = 229
mem[63045] = 833068
mem[62987] = 4856476
mem[55200] = 547
mask = 10X1X0X00X101XX01X101000101X101X0000
mem[48550] = 259751485
mem[16431] = 980596
mem[39021] = 106388364
mem[48142] = 3777
mem[17652] = 434163
mem[18613] = 19486709
mem[26579] = 385135647
mask = 0X0X11001XXXX00X011000X111001010X000
mem[38574] = 3154
mem[12071] = 235510356
mem[61159] = 109679
mem[47527] = 121608755
mem[55803] = 170459
mask = 011001110111001X0X1X110000011X0010X1
mem[59793] = 558
mem[23056] = 115523
mem[33423] = 313
mem[38854] = 925458
mask = X1XX0010111011X01100X001X00X0110111X
mem[14936] = 12443259
mem[61314] = 1154
mem[20124] = 276418611
mem[60294] = 5865720
mem[59584] = 395879
mem[22227] = 51398
mem[29560] = 25396
mask = X1X11010X1101X0001100010X000110X0111
mem[4256] = 699
mem[15784] = 208
mem[54321] = 35269104
mem[65299] = 41354
mem[42445] = 501
mask = 01111X110010000X01001100X0000000001X
mem[28748] = 8402
mem[9833] = 7040"

// https://stackoverflow.com/a/3722671/403702
let (|Prefix|_|) (p: string) (s: string) =
    if s.StartsWith(p) then Some(s.Substring(p.Length)) else None

type Instruction =
    | SetMask of (int * char) list
    | WriteValue of int64 * int64

let parseLine (line: string): Instruction =
    match line with
    | Prefix "mask = " rest ->
        let mask =
            rest
            |> String.toList
            |> List.indexed
            |> List.map (fun (i, char) ->
                let newI = rest.Length - i - 1
                match char with
                | 'X' -> (newI, char)
                | '1' -> (newI, char)
                | '0' -> (newI, char)
                | _ -> failwithf "weird mask bit %A" char)

        SetMask mask
    | Prefix "mem[" rest ->
        let address =
            String.takeWhile System.Char.IsDigit rest

        match String.drop address.Length rest with
        | Prefix "] = " num -> WriteValue(int64 address, int64 num)
        | _ -> failwithf "weird mem line %A" line
    | _ -> failwithf "weird line %A" line

let parse (input: string): Instruction list =
    input.Split "\n"
    |> Array.map parseLine
    |> Array.toList

type State =
    { Mask: (int * char) list
      Mem: Map<int64, int64> }

let applyMask (mask: (int * char) list) (int: int64): int64 =
    mask
    |> List.fold (fun n (index, char) ->
        match char with
        | '1' -> n ||| (1L <<< index) // set bit to 1
        | '0' -> n &&& ~~~(1L <<< index) // set bit to 0
        | _ -> n) int

let applyInstruction1 (instruction: Instruction) (state: State): State =
    match instruction with
    | SetMask mask -> { state with Mask = mask }
    | WriteValue (address, value) ->
        { state with
              Mem = Map.add address (applyMask state.Mask value) state.Mem }

let part1 (instructions: Instruction list): int64 =
    instructions
    |> List.fold (fun mem instruction -> applyInstruction1 instruction mem)
           { Mask = []; Mem = Map.empty }
    |> fun state -> state.Mem
    |> Map.values
    |> Seq.sum

// http://www.fssnip.net/2A/title/Cartesian-product-of-n-lists
let rec cartesian lstlst =
    match lstlst with
    | h :: [] -> List.fold (fun acc elem -> [ elem ] :: acc) [] h
    | h :: t ->
        List.fold (fun cacc celem ->
            (List.fold (fun acc elem -> (elem :: celem) :: acc) [] h)
            @ cacc) [] (cartesian t)
    | _ -> []


let applyFloatingMask (address: int64)
                      (value: int64)
                      (mask: (int * char) list)
                      (mem: Map<int64, int64>)
                      : Map<int64, int64> =
    let addressAfterNonfloatingMask: int64 =
        mask
        |> List.fold (fun n (index, char) ->
            match char with
            | '1' -> n ||| (1L <<< index) // set bit to 1
            | '0' -> n // leave unchanged
            | 'X' -> n // ignore for now
            | _ -> failwithf "wut %A" char) address

    let floatingIndexes: int list =
        List.choose (fun (index, char) ->
            if char = 'X' then Some index else None) mask

    let floatingIndexCombinations: (int * bool) list list =
        [ true; false ]
        |> List.replicate floatingIndexes.Length
        |> cartesian
        |> List.map (List.zip floatingIndexes)

    let allAddresses: int64 list =
        addressAfterNonfloatingMask (* all floating Xs = 0s *)
        :: (floatingIndexCombinations
            |> List.map (fun indexesToBeSet ->
                indexesToBeSet
                |> List.fold (fun n (index, setToOne) ->
                    if setToOne then
                        n ||| (1L <<< index) // set bit to 1
                    else
                        n &&& ~~~(1L <<< index) // set bit to 0

                    ) addressAfterNonfloatingMask))

    allAddresses
    |> List.fold (fun m address -> Map.add address value m) mem

let applyInstruction2 (instruction: Instruction) (state: State): State =
    match instruction with
    | SetMask mask -> { state with Mask = mask }
    | WriteValue (address, value) ->
        { state with
              Mem = applyFloatingMask address value state.Mask state.Mem }

let part2 (instructions: Instruction list): int64 =
    instructions
    |> List.fold (fun mem instruction -> applyInstruction2 instruction mem)
           { Mask = []; Mem = Map.empty }
    |> fun state -> state.Mem
    |> Map.values
    |> Seq.sum

let main = realInput |> parse |> part2
