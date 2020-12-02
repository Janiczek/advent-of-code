module AoC.Day02

type Policy = { Min: int; Max: int; Letter: char }

type Row = { Policy: Policy; Password: string }

let exampleInput: Row list =
    [ { Policy = { Min = 1; Max = 3; Letter = 'a' }
        Password = "abcde" }
      { Policy = { Min = 1; Max = 3; Letter = 'b' }
        Password = "cdefg" }
      { Policy = { Min = 2; Max = 9; Letter = 'c' }
        Password = "ccccccccc" } ]

let realInput: Row list =
    [| { Policy = { Min = 8; Max = 11; Letter = 'l' }
         Password = "qllllqllklhlvtl" }
       { Policy = { Min = 1; Max = 3; Letter = 'm' }
         Password = "wmmmmmttm" }
       { Policy = { Min = 2; Max = 4; Letter = 'p' }
         Password = "pgppp" }
       { Policy = { Min = 11; Max = 12; Letter = 'n' }
         Password = "nnndnnnnnnnn" }
       { Policy = { Min = 17; Max = 19; Letter = 'q' }
         Password = "qprqdcgrqrqmmhtqqvr" }
       { Policy = { Min = 16; Max = 17; Letter = 'k' }
         Password = "nphkpzqswcltkkbkk" }
       { Policy = { Min = 6; Max = 9; Letter = 'c' }
         Password = "rvcvlcjcbhxs" }
       { Policy = { Min = 18; Max = 20; Letter = 'v' }
         Password = "hbjhmrtwzfqfvhzjjvcv" }
       { Policy = { Min = 5; Max = 9; Letter = 'z' }
         Password = "jzzhzttttnz" }
       { Policy = { Min = 7; Max = 13; Letter = 'd' }
         Password = "bdqdtddddnwdd" }
       { Policy = { Min = 9; Max = 11; Letter = 'd' }
         Password = "ddddddddxdldddd" }
       { Policy = { Min = 6; Max = 10; Letter = 'f' }
         Password = "fblhfdztgf" }
       { Policy = { Min = 2; Max = 11; Letter = 'b' }
         Password = "vszxfnwghcb" }
       { Policy = { Min = 15; Max = 18; Letter = 'n' }
         Password = "nbnmwxnnlkmlknnnhn" }
       { Policy = { Min = 2; Max = 9; Letter = 'z' }
         Password = "lhwqvczrrqqhqlfvkbcm" }
       { Policy = { Min = 15; Max = 16; Letter = 'd' }
         Password = "dndddddddjdddddbdld" }
       { Policy = { Min = 7; Max = 8; Letter = 'k' }
         Password = "kkkmkkkf" }
       { Policy = { Min = 1; Max = 8; Letter = 'p' }
         Password = "rdcmrkbwqjpph" }
       { Policy = { Min = 2; Max = 6; Letter = 's' }
         Password = "cswdpsjgsfvzkvqqmrqf" }
       { Policy = { Min = 9; Max = 11; Letter = 'm' }
         Password = "mmmmmmmzbmmmv" }
       { Policy = { Min = 8; Max = 9; Letter = 'j' }
         Password = "jjjjjjjjfj" }
       { Policy = { Min = 7; Max = 8; Letter = 'd' }
         Password = "dddsjnds" }
       { Policy = { Min = 1; Max = 4; Letter = 'f' }
         Password = "qffb" }
       { Policy = { Min = 3; Max = 8; Letter = 'f' }
         Password = "cphmtfff" }
       { Policy = { Min = 1; Max = 13; Letter = 's' }
         Password = "rjsscssstsvssss" }
       { Policy = { Min = 9; Max = 14; Letter = 's' }
         Password = "gtsnlbqnckhxmssbbs" }
       { Policy = { Min = 12; Max = 14; Letter = 'j' }
         Password = "jfjnjbjrjpdndj" }
       { Policy = { Min = 15; Max = 16; Letter = 't' }
         Password = "tttttttttttttttwt" }
       { Policy = { Min = 7; Max = 8; Letter = 'r' }
         Password = "rgrdrrrrrrrrjhrrrrrr" }
       { Policy = { Min = 5; Max = 8; Letter = 't' }
         Password = "lpcqfgzttlt" }
       { Policy = { Min = 1; Max = 12; Letter = 'r' }
         Password = "wrrrrrrjrrrrrrrr" }
       { Policy = { Min = 14; Max = 19; Letter = 'd' }
         Password = "ddvcdddddddhddprldl" }
       { Policy = { Min = 4; Max = 8; Letter = 'd' }
         Password = "pkddlzxsl" }
       { Policy = { Min = 7; Max = 11; Letter = 'x' }
         Password = "xhxqxcfkxwxxnm" }
       { Policy = { Min = 3; Max = 7; Letter = 'q' }
         Password = "qqqqqqjqqqd" }
       { Policy = { Min = 3; Max = 13; Letter = 's' }
         Password = "rtzsktsdfhtbs" }
       { Policy = { Min = 8; Max = 15; Letter = 'n' }
         Password = "nnnnnnnnknnnnnsnnn" }
       { Policy = { Min = 10; Max = 13; Letter = 'r' }
         Password = "rrrrrrrrrrrrnr" }
       { Policy = { Min = 1; Max = 9; Letter = 'r' }
         Password = "ldfdgzprnptrd" }
       { Policy = { Min = 2; Max = 3; Letter = 'k' }
         Password = "rqkthj" }
       { Policy = { Min = 4; Max = 7; Letter = 'p' }
         Password = "prrpswdpnmpxmjzsp" }
       { Policy = { Min = 12; Max = 13; Letter = 'p' }
         Password = "pmwbptnpppjprfpkppgj" }
       { Policy = { Min = 4; Max = 6; Letter = 'w' }
         Password = "cfwdlw" }
       { Policy = { Min = 2; Max = 9; Letter = 'r' }
         Password = "pnnvrfjhz" }
       { Policy = { Min = 14; Max = 16; Letter = 'b' }
         Password = "bbbbbbbbbbbbbtbbb" }
       { Policy = { Min = 2; Max = 7; Letter = 'l' }
         Password = "xlmzgklxljcl" }
       { Policy = { Min = 1; Max = 6; Letter = 'c' }
         Password = "cccccccc" }
       { Policy = { Min = 11; Max = 12; Letter = 'w' }
         Password = "dmpzfzpwwnwwpggw" }
       { Policy = { Min = 2; Max = 3; Letter = 'c' }
         Password = "xrccccmcc" }
       { Policy = { Min = 12; Max = 13; Letter = 'k' }
         Password = "kkkkkkkvkkkvkknkkk" }
       { Policy = { Min = 10; Max = 12; Letter = 'h' }
         Password = "hhnhcvhhhqhh" }
       { Policy = { Min = 17; Max = 18; Letter = 'd' }
         Password = "dddjddbdzdddvddddw" }
       { Policy = { Min = 1; Max = 5; Letter = 'p' }
         Password = "pppphp" }
       { Policy = { Min = 11; Max = 13; Letter = 'v' }
         Password = "fvvvvjlvbvrvdhbvv" }
       { Policy = { Min = 10; Max = 14; Letter = 'b' }
         Password = "bzxxqcgqnbkmhm" }
       { Policy = { Min = 1; Max = 14; Letter = 'g' }
         Password = "xggghgngqnggggggggg" }
       { Policy = { Min = 9; Max = 10; Letter = 's' }
         Password = "xslsmpfnxvvssqmgf" }
       { Policy = { Min = 16; Max = 17; Letter = 's' }
         Password = "nfqggjzbfsssllwns" }
       { Policy = { Min = 9; Max = 10; Letter = 'w' }
         Password = "wwwwwwwwfw" }
       { Policy = { Min = 13; Max = 15; Letter = 'z' }
         Password = "zzzjzzzjzzzzzzgz" }
       { Policy = { Min = 7; Max = 10; Letter = 'n' }
         Password = "jfnwgwnnnn" }
       { Policy = { Min = 4; Max = 5; Letter = 'b' }
         Password = "btbqb" }
       { Policy = { Min = 1; Max = 5; Letter = 'w' }
         Password = "vwflw" }
       { Policy = { Min = 15; Max = 16; Letter = 'v' }
         Password = "vvxvdvvvvzvxmhxvv" }
       { Policy = { Min = 3; Max = 4; Letter = 'b' }
         Password = "cgbbqk" }
       { Policy = { Min = 1; Max = 3; Letter = 'f' }
         Password = "bffffdfclfffgfkf" }
       { Policy = { Min = 6; Max = 11; Letter = 'm' }
         Password = "xckgmdcqmwk" }
       { Policy = { Min = 6; Max = 9; Letter = 'c' }
         Password = "vcptncxbcg" }
       { Policy = { Min = 5; Max = 6; Letter = 'm' }
         Password = "mmmmdm" }
       { Policy = { Min = 2; Max = 3; Letter = 'd' }
         Password = "dmdd" }
       { Policy = { Min = 5; Max = 7; Letter = 'v' }
         Password = "vvhrxkd" }
       { Policy = { Min = 7; Max = 10; Letter = 'b' }
         Password = "bbbbbbqbbbbb" }
       { Policy = { Min = 8; Max = 9; Letter = 'm' }
         Password = "mhvmmwlgm" }
       { Policy = { Min = 3; Max = 4; Letter = 'x' }
         Password = "xvtxkz" }
       { Policy = { Min = 3; Max = 9; Letter = 'w' }
         Password = "wwlwwwwwkqww" }
       { Policy = { Min = 4; Max = 18; Letter = 'g' }
         Password = "mxslljzcgpwsrggqqc" }
       { Policy = { Min = 2; Max = 3; Letter = 'x' }
         Password = "xcff" }
       { Policy = { Min = 16; Max = 17; Letter = 'j' }
         Password = "jjjjjjjjjjfqjfjwhjjj" }
       { Policy = { Min = 9; Max = 13; Letter = 'p' }
         Password = "ppppppppgppppp" }
       { Policy = { Min = 16; Max = 17; Letter = 'f' }
         Password = "fffffffffffffffffff" }
       { Policy = { Min = 3; Max = 5; Letter = 'c' }
         Password = "cncpcck" }
       { Policy = { Min = 9; Max = 11; Letter = 'c' }
         Password = "kzcwczccccmcfsrcc" }
       { Policy = { Min = 3; Max = 7; Letter = 's' }
         Password = "ssdsssvnsssssjs" }
       { Policy = { Min = 1; Max = 6; Letter = 'v' }
         Password = "vvvvqv" }
       { Policy = { Min = 5; Max = 7; Letter = 'b' }
         Password = "fzbbxbbbgbb" }
       { Policy = { Min = 3; Max = 9; Letter = 't' }
         Password = "gtttttttftt" }
       { Policy = { Min = 5; Max = 19; Letter = 'p' }
         Password = "ngpnpklwsclptfjvtgm" }
       { Policy = { Min = 3; Max = 4; Letter = 'd' }
         Password = "dddtdddddd" }
       { Policy = { Min = 4; Max = 5; Letter = 'm' }
         Password = "mjmmwl" }
       { Policy = { Min = 11; Max = 13; Letter = 'l' }
         Password = "lllblllllvllrl" }
       { Policy = { Min = 2; Max = 6; Letter = 'h' }
         Password = "cphqvz" }
       { Policy = { Min = 17; Max = 19; Letter = 'w' }
         Password = "gwwrvfglsljwfgxwbbw" }
       { Policy = { Min = 15; Max = 17; Letter = 'x' }
         Password = "gfcxzcwgjmkwfqxrxzrd" }
       { Policy = { Min = 13; Max = 14; Letter = 'w' }
         Password = "lrmhhxwfkwnkwnbsq" }
       { Policy = { Min = 7; Max = 8; Letter = 'f' }
         Password = "ffffffgcff" }
       { Policy = { Min = 11; Max = 19; Letter = 'v' }
         Password = "vvvvvvvvvvzvvvvvvvlv" }
       { Policy = { Min = 7; Max = 8; Letter = 'k' }
         Password = "kxkkkkpk" }
       { Policy = { Min = 7; Max = 14; Letter = 'v' }
         Password = "vfvvvskttcvvvvvfvv" }
       { Policy = { Min = 11; Max = 12; Letter = 'm' }
         Password = "mmmdhmmgmkgmjmr" }
       { Policy = { Min = 1; Max = 7; Letter = 'p' }
         Password = "hpppppppppbnc" }
       { Policy = { Min = 3; Max = 8; Letter = 'n' }
         Password = "rttbbpjnmzn" }
       { Policy = { Min = 8; Max = 9; Letter = 'n' }
         Password = "nrfnnvxrp" }
       { Policy = { Min = 3; Max = 4; Letter = 'x' }
         Password = "tnxnngq" }
       { Policy = { Min = 9; Max = 12; Letter = 's' }
         Password = "mbhsxshssrtwvm" }
       { Policy = { Min = 11; Max = 15; Letter = 'n' }
         Password = "nwmnlhgjnnptkmn" }
       { Policy = { Min = 1; Max = 4; Letter = 'x' }
         Password = "xmxl" }
       { Policy = { Min = 1; Max = 6; Letter = 'f' }
         Password = "bffffffffff" }
       { Policy = { Min = 2; Max = 4; Letter = 'r' }
         Password = "zrrr" }
       { Policy = { Min = 2; Max = 3; Letter = 't' }
         Password = "ttmwvt" }
       { Policy = { Min = 3; Max = 5; Letter = 'n' }
         Password = "ngnnr" }
       { Policy = { Min = 13; Max = 17; Letter = 'p' }
         Password = "jtpppfgklkpshpndpp" }
       { Policy = { Min = 1; Max = 7; Letter = 'r' }
         Password = "rrrrrrrrrrrr" }
       { Policy = { Min = 7; Max = 10; Letter = 'h' }
         Password = "hmhhhhhhhzhhhhhhh" }
       { Policy = { Min = 9; Max = 15; Letter = 'l' }
         Password = "glxqscckgxtkzfllk" }
       { Policy = { Min = 2; Max = 3; Letter = 's' }
         Password = "gfsh" }
       { Policy = { Min = 5; Max = 6; Letter = 'b' }
         Password = "dpphbj" }
       { Policy = { Min = 6; Max = 13; Letter = 'h' }
         Password = "hhhhhhhhhhhbshgh" }
       { Policy = { Min = 5; Max = 13; Letter = 'd' }
         Password = "dddpbddddddddddd" }
       { Policy = { Min = 10; Max = 11; Letter = 'p' }
         Password = "pppppppppdppppp" }
       { Policy = { Min = 5; Max = 7; Letter = 'j' }
         Password = "jjjjjcx" }
       { Policy = { Min = 8; Max = 9; Letter = 'r' }
         Password = "rrrrmrrrm" }
       { Policy = { Min = 1; Max = 3; Letter = 'f' }
         Password = "ffff" }
       { Policy = { Min = 8; Max = 15; Letter = 'b' }
         Password = "bbbbbbbgbbbbbbrb" }
       { Policy = { Min = 3; Max = 6; Letter = 'h' }
         Password = "hhrhhhhhhh" }
       { Policy = { Min = 9; Max = 10; Letter = 'v' }
         Password = "tkwvvvjvvvblvxvxhxvv" }
       { Policy = { Min = 5; Max = 9; Letter = 't' }
         Password = "ttttttttmtf" }
       { Policy = { Min = 1; Max = 4; Letter = 'm' }
         Password = "fmmmmm" }
       { Policy = { Min = 5; Max = 9; Letter = 'n' }
         Password = "nfnlnkblnnfnxtzn" }
       { Policy = { Min = 3; Max = 11; Letter = 'v' }
         Password = "dnsvvvbvnvvxvj" }
       { Policy = { Min = 8; Max = 13; Letter = 'h' }
         Password = "nfgmbfjhdhlhb" }
       { Policy = { Min = 2; Max = 14; Letter = 'q' }
         Password = "cxtgcrpsxnjshlqbh" }
       { Policy = { Min = 5; Max = 13; Letter = 'b' }
         Password = "bbbbjbbbbbbbbbbbbbb" }
       { Policy = { Min = 4; Max = 5; Letter = 'l' }
         Password = "wldljllcl" }
       { Policy = { Min = 19; Max = 20; Letter = 'b' }
         Password = "jbvbbbbbqqbbbbbbbbbs" }
       { Policy = { Min = 5; Max = 14; Letter = 'b' }
         Password = "bbbbbbbbbbbbbbbbb" }
       { Policy = { Min = 2; Max = 5; Letter = 'q' }
         Password = "qmqjqhfk" }
       { Policy = { Min = 12; Max = 13; Letter = 's' }
         Password = "sssfsnsssssssxsms" }
       { Policy = { Min = 3; Max = 17; Letter = 'z' }
         Password = "zzzzzzzzzzzzzzzznzz" }
       { Policy = { Min = 13; Max = 14; Letter = 't' }
         Password = "ttztttstttbttb" }
       { Policy = { Min = 2; Max = 6; Letter = 'q' }
         Password = "vqhhrqlgvckvsrpwmqwz" }
       { Policy = { Min = 15; Max = 16; Letter = 'd' }
         Password = "dddddddddddddddhd" }
       { Policy = { Min = 5; Max = 6; Letter = 'w' }
         Password = "wwwwdwww" }
       { Policy = { Min = 1; Max = 2; Letter = 'f' }
         Password = "jffkf" }
       { Policy = { Min = 6; Max = 10; Letter = 'j' }
         Password = "jjjjjrjwjbjxgpjjjm" }
       { Policy = { Min = 4; Max = 5; Letter = 'c' }
         Password = "cvgccmcqzrcd" }
       { Policy = { Min = 3; Max = 7; Letter = 'h' }
         Password = "mghkhfgzmkz" }
       { Policy = { Min = 10; Max = 17; Letter = 'f' }
         Password = "ffffsfffffffpffwkff" }
       { Policy = { Min = 4; Max = 9; Letter = 'g' }
         Password = "ggbgcgjgggggg" }
       { Policy = { Min = 8; Max = 11; Letter = 'd' }
         Password = "mdnddhpddddm" }
       { Policy = { Min = 6; Max = 7; Letter = 'c' }
         Password = "ctcldgc" }
       { Policy = { Min = 13; Max = 14; Letter = 'm' }
         Password = "mmmmmmmmmmmmlmm" }
       { Policy = { Min = 18; Max = 19; Letter = 't' }
         Password = "ttttttttptttttnttgt" }
       { Policy = { Min = 7; Max = 8; Letter = 'g' }
         Password = "qngggtghnxggs" }
       { Policy = { Min = 3; Max = 12; Letter = 'c' }
         Password = "scccrbjtdccq" }
       { Policy = { Min = 2; Max = 3; Letter = 'q' }
         Password = "qgqjq" }
       { Policy = { Min = 5; Max = 6; Letter = 'x' }
         Password = "xxxxzxxxxx" }
       { Policy = { Min = 6; Max = 10; Letter = 'v' }
         Password = "nwjsxvzhvgmsglftbpvc" }
       { Policy = { Min = 2; Max = 5; Letter = 'm' }
         Password = "xjtbsffdwmxmhxrmpm" }
       { Policy = { Min = 4; Max = 7; Letter = 'c' }
         Password = "mfgcvqccg" }
       { Policy = { Min = 1; Max = 7; Letter = 'c' }
         Password = "cctcccc" }
       { Policy = { Min = 3; Max = 4; Letter = 'm' }
         Password = "cnnw" }
       { Policy = { Min = 10; Max = 13; Letter = 's' }
         Password = "sstssssszsssss" }
       { Policy = { Min = 9; Max = 10; Letter = 'h' }
         Password = "hchhhhjhdh" }
       { Policy = { Min = 2; Max = 4; Letter = 'j' }
         Password = "xjjgsz" }
       { Policy = { Min = 4; Max = 11; Letter = 'b' }
         Password = "rbblbbpmbmbdbjgcbhk" }
       { Policy = { Min = 10; Max = 11; Letter = 'r' }
         Password = "rrrrrrrrrjrrr" }
       { Policy = { Min = 3; Max = 6; Letter = 'g' }
         Password = "hvgfzgjrkdf" }
       { Policy = { Min = 1; Max = 10; Letter = 'b' }
         Password = "tbbbbbbpbbbbbb" }
       { Policy = { Min = 12; Max = 13; Letter = 'n' }
         Password = "nhnnnnnnnnjnznnrnrxl" }
       { Policy = { Min = 7; Max = 13; Letter = 'w' }
         Password = "wwmwfncfwdxww" }
       { Policy = { Min = 3; Max = 4; Letter = 'c' }
         Password = "wccq" }
       { Policy = { Min = 7; Max = 16; Letter = 'x' }
         Password = "xxxxxxtxxxxxxxxxx" }
       { Policy = { Min = 3; Max = 7; Letter = 'k' }
         Password = "zmhkssxs" }
       { Policy = { Min = 1; Max = 8; Letter = 'n' }
         Password = "gnnnnnnnnn" }
       { Policy = { Min = 13; Max = 14; Letter = 'r' }
         Password = "rrrrrrrrrrrrmr" }
       { Policy = { Min = 2; Max = 3; Letter = 'r' }
         Password = "rrlr" }
       { Policy = { Min = 3; Max = 4; Letter = 'c' }
         Password = "sccccwvpjpplgctg" }
       { Policy = { Min = 1; Max = 2; Letter = 'b' }
         Password = "svbs" }
       { Policy = { Min = 1; Max = 2; Letter = 'f' }
         Password = "fbfffrff" }
       { Policy = { Min = 6; Max = 14; Letter = 'l' }
         Password = "lllllvlllllxtllqllll" }
       { Policy = { Min = 13; Max = 14; Letter = 'f' }
         Password = "ffffffffkfffjf" }
       { Policy = { Min = 1; Max = 6; Letter = 'z' }
         Password = "zzzrzfzzzzm" }
       { Policy = { Min = 4; Max = 8; Letter = 'k' }
         Password = "kzzktkgrzjdkq" }
       { Policy = { Min = 6; Max = 8; Letter = 'j' }
         Password = "jjjfsgjjbt" }
       { Policy = { Min = 3; Max = 7; Letter = 'q' }
         Password = "qtqqqqq" }
       { Policy = { Min = 17; Max = 18; Letter = 'x' }
         Password = "djxqkrlcwxxxlvhjxh" }
       { Policy = { Min = 6; Max = 8; Letter = 'c' }
         Password = "tqpcgcjc" }
       { Policy = { Min = 2; Max = 13; Letter = 'f' }
         Password = "khqhkkszblvffhfwcg" }
       { Policy = { Min = 7; Max = 18; Letter = 'p' }
         Password = "kjxrtcpptzpxddbkts" }
       { Policy = { Min = 5; Max = 12; Letter = 'l' }
         Password = "llldllllllntm" }
       { Policy = { Min = 2; Max = 7; Letter = 'q' }
         Password = "bvvrnhqhpqw" }
       { Policy = { Min = 2; Max = 6; Letter = 's' }
         Password = "scssswssss" }
       { Policy = { Min = 1; Max = 2; Letter = 'n' }
         Password = "njdnnnn" }
       { Policy = { Min = 4; Max = 8; Letter = 'h' }
         Password = "hhhzhhhhhhhhhhhhhhh" }
       { Policy = { Min = 9; Max = 10; Letter = 'j' }
         Password = "jjjjjjjjkjjxqjjw" }
       { Policy = { Min = 4; Max = 9; Letter = 't' }
         Password = "tzdqtlttttktttttcttt" }
       { Policy = { Min = 4; Max = 5; Letter = 'w' }
         Password = "wdwht" }
       { Policy = { Min = 8; Max = 9; Letter = 'x' }
         Password = "xfxxxxfrsxp" }
       { Policy = { Min = 3; Max = 4; Letter = 'm' }
         Password = "mmmmgwwbmztpmbmmmtls" }
       { Policy = { Min = 8; Max = 17; Letter = 'f' }
         Password = "mgzhfgfffswbgnvbc" }
       { Policy = { Min = 5; Max = 6; Letter = 't' }
         Password = "tttttg" }
       { Policy = { Min = 4; Max = 18; Letter = 'x' }
         Password = "xxxwxxxxxxxbxxxnxxx" }
       { Policy = { Min = 2; Max = 4; Letter = 'l' }
         Password = "cqglmhmtjls" }
       { Policy = { Min = 2; Max = 4; Letter = 'z' }
         Password = "zfgzr" }
       { Policy = { Min = 11; Max = 18; Letter = 'k' }
         Password = "nrdngbjkpckjxwdbrh" }
       { Policy = { Min = 8; Max = 9; Letter = 'f' }
         Password = "fffffffpf" }
       { Policy = { Min = 4; Max = 7; Letter = 'g' }
         Password = "ggtgghgsnggr" }
       { Policy = { Min = 5; Max = 6; Letter = 'w' }
         Password = "wswwlw" }
       { Policy = { Min = 3; Max = 5; Letter = 'b' }
         Password = "dfbbjbccx" }
       { Policy = { Min = 2; Max = 3; Letter = 't' }
         Password = "tdwfzg" }
       { Policy = { Min = 4; Max = 5; Letter = 'r' }
         Password = "rhrbw" }
       { Policy = { Min = 4; Max = 5; Letter = 'j' }
         Password = "jjjjtjjn" }
       { Policy = { Min = 3; Max = 4; Letter = 'k' }
         Password = "hkzkk" }
       { Policy = { Min = 1; Max = 2; Letter = 'c' }
         Password = "cdzccc" }
       { Policy = { Min = 3; Max = 5; Letter = 'r' }
         Password = "vhlrrvhr" }
       { Policy = { Min = 4; Max = 9; Letter = 'l' }
         Password = "qbjdqldwzdl" }
       { Policy = { Min = 6; Max = 11; Letter = 'f' }
         Password = "flvpvfcfrgg" }
       { Policy = { Min = 17; Max = 18; Letter = 'g' }
         Password = "xgggggghgggglggggz" }
       { Policy = { Min = 5; Max = 6; Letter = 'v' }
         Password = "bhvgvl" }
       { Policy = { Min = 7; Max = 15; Letter = 'n' }
         Password = "nnnnnnnnnnnnnngnnnnn" }
       { Policy = { Min = 6; Max = 15; Letter = 'd' }
         Password = "dtddsddfddmcpdf" }
       { Policy = { Min = 10; Max = 12; Letter = 'b' }
         Password = "bbbbbbbbvlbbbbbbb" }
       { Policy = { Min = 2; Max = 12; Letter = 'd' }
         Password = "szdghlzwxpnd" }
       { Policy = { Min = 3; Max = 4; Letter = 'z' }
         Password = "zzzw" }
       { Policy = { Min = 6; Max = 15; Letter = 't' }
         Password = "mrnjvfhtlqwlfzt" }
       { Policy = { Min = 8; Max = 10; Letter = 'x' }
         Password = "lcxcbrxxjw" }
       { Policy = { Min = 6; Max = 13; Letter = 'r' }
         Password = "rgszrlzmmlpdngchhxz" }
       { Policy = { Min = 1; Max = 14; Letter = 'g' }
         Password = "fgggggggggggggggggg" }
       { Policy = { Min = 3; Max = 5; Letter = 'n' }
         Password = "znnnjpksqtzt" }
       { Policy = { Min = 17; Max = 18; Letter = 'l' }
         Password = "bcfmqlsltppxwsxslb" }
       { Policy = { Min = 5; Max = 8; Letter = 't' }
         Password = "tttnctttt" }
       { Policy = { Min = 4; Max = 5; Letter = 'm' }
         Password = "mmmmnmml" }
       { Policy = { Min = 15; Max = 20; Letter = 'k' }
         Password = "kmkpvxkgnckknzkpkqkt" }
       { Policy = { Min = 12; Max = 15; Letter = 'x' }
         Password = "xxxxvxlxxkdxxxx" }
       { Policy = { Min = 1; Max = 5; Letter = 'v' }
         Password = "vjdndsvsjvqzvnv" }
       { Policy = { Min = 1; Max = 4; Letter = 'k' }
         Password = "kkkkkkk" }
       { Policy = { Min = 5; Max = 8; Letter = 'c' }
         Password = "cxccccccc" }
       { Policy = { Min = 2; Max = 4; Letter = 'v' }
         Password = "vdvd" }
       { Policy = { Min = 12; Max = 14; Letter = 'w' }
         Password = "wwwwwwwwwwqwwbww" }
       { Policy = { Min = 2; Max = 3; Letter = 'b' }
         Password = "kbbtbrllwp" }
       { Policy = { Min = 14; Max = 15; Letter = 'f' }
         Password = "ffffffffffffffm" }
       { Policy = { Min = 9; Max = 10; Letter = 'v' }
         Password = "nlngldlnvsbwcvvt" }
       { Policy = { Min = 4; Max = 5; Letter = 'c' }
         Password = "kccxccc" }
       { Policy = { Min = 5; Max = 9; Letter = 'm' }
         Password = "bmmmmmmmzmf" }
       { Policy = { Min = 8; Max = 9; Letter = 'f' }
         Password = "fzfvffjffffv" }
       { Policy = { Min = 3; Max = 14; Letter = 'r' }
         Password = "rrqrrrrrrrrrrjrh" }
       { Policy = { Min = 13; Max = 14; Letter = 'p' }
         Password = "ppptpppppptpqdpm" }
       { Policy = { Min = 6; Max = 12; Letter = 't' }
         Password = "tttjtvtgcwvttttqkt" }
       { Policy = { Min = 5; Max = 10; Letter = 'z' }
         Password = "glsrzctzzz" }
       { Policy = { Min = 5; Max = 8; Letter = 's' }
         Password = "gckwcshsl" }
       { Policy = { Min = 17; Max = 19; Letter = 'n' }
         Password = "nnnnhnnnnnnnntwnnnd" }
       { Policy = { Min = 7; Max = 9; Letter = 'r' }
         Password = "rrrrfdrrxrrrrrrrrrrq" }
       { Policy = { Min = 17; Max = 20; Letter = 'm' }
         Password = "rmxbmmvwphmxmzlmbmxm" }
       { Policy = { Min = 14; Max = 15; Letter = 'l' }
         Password = "kkjwtlsrlhltmdl" }
       { Policy = { Min = 14; Max = 17; Letter = 'f' }
         Password = "ffffffffffffffffff" }
       { Policy = { Min = 3; Max = 13; Letter = 'l' }
         Password = "khxtqtwbvpmgll" }
       { Policy = { Min = 2; Max = 4; Letter = 'w' }
         Password = "wwbmww" }
       { Policy = { Min = 9; Max = 15; Letter = 'n' }
         Password = "nwxcnxnckttrkdqnn" }
       { Policy = { Min = 3; Max = 4; Letter = 't' }
         Password = "fgnwjbtlntsr" }
       { Policy = { Min = 15; Max = 16; Letter = 'x' }
         Password = "mcxxxxxxrxxxpczx" }
       { Policy = { Min = 6; Max = 16; Letter = 'w' }
         Password = "vtcvkmrwvlmwdvrwmqj" }
       { Policy = { Min = 1; Max = 3; Letter = 'c' }
         Password = "mmcjckwn" }
       { Policy = { Min = 1; Max = 10; Letter = 'c' }
         Password = "ccccccccckccccc" }
       { Policy = { Min = 14; Max = 16; Letter = 'l' }
         Password = "kqjhpjgzvxlnxxll" }
       { Policy = { Min = 4; Max = 7; Letter = 'r' }
         Password = "xrtrrrrrcrrmrrrr" }
       { Policy = { Min = 8; Max = 13; Letter = 'm' }
         Password = "mmmmmmmmmmmmcmm" }
       { Policy = { Min = 7; Max = 8; Letter = 'r' }
         Password = "trkrrrrwf" }
       { Policy = { Min = 3; Max = 4; Letter = 'n' }
         Password = "pnjn" }
       { Policy = { Min = 1; Max = 5; Letter = 'k' }
         Password = "skkknkk" }
       { Policy = { Min = 11; Max = 16; Letter = 'k' }
         Password = "kfkkkjkqpqgkzkkkkwsn" }
       { Policy = { Min = 13; Max = 17; Letter = 'f' }
         Password = "gdllffxlxwncljgwf" }
       { Policy = { Min = 3; Max = 5; Letter = 's' }
         Password = "gwspdtjtnlbsfffvhlg" }
       { Policy = { Min = 15; Max = 17; Letter = 'm' }
         Password = "krmfcsqbmmmjwgkdmm" }
       { Policy = { Min = 13; Max = 14; Letter = 'l' }
         Password = "knhdrdzcmdhlll" }
       { Policy = { Min = 2; Max = 3; Letter = 'p' }
         Password = "frps" }
       { Policy = { Min = 2; Max = 9; Letter = 'z' }
         Password = "lzwnzmvnqgkpbxv" }
       { Policy = { Min = 5; Max = 9; Letter = 'n' }
         Password = "nnngrnnbj" }
       { Policy = { Min = 3; Max = 5; Letter = 'c' }
         Password = "mncnbk" }
       { Policy = { Min = 2; Max = 5; Letter = 'n' }
         Password = "djgnnnnnzbnnnx" }
       { Policy = { Min = 7; Max = 8; Letter = 'v' }
         Password = "vvgvgvvm" }
       { Policy = { Min = 5; Max = 15; Letter = 'w' }
         Password = "wwwwwwwwwwwwwwwwwww" }
       { Policy = { Min = 6; Max = 7; Letter = 'd' }
         Password = "dddddcz" }
       { Policy = { Min = 7; Max = 9; Letter = 'g' }
         Password = "glrgcggvgckrgggz" }
       { Policy = { Min = 2; Max = 3; Letter = 'n' }
         Password = "dnwbnc" }
       { Policy = { Min = 6; Max = 8; Letter = 't' }
         Password = "lttztzqt" }
       { Policy = { Min = 1; Max = 4; Letter = 'm' }
         Password = "mmmxm" }
       { Policy = { Min = 4; Max = 14; Letter = 'l' }
         Password = "qqhgtftklcnmllcbgbrx" }
       { Policy = { Min = 2; Max = 3; Letter = 'd' }
         Password = "sdnk" }
       { Policy = { Min = 12; Max = 15; Letter = 'l' }
         Password = "lqllljfglvldcql" }
       { Policy = { Min = 2; Max = 10; Letter = 'k' }
         Password = "kgkkkkkkkkxkkkkkkkkk" }
       { Policy = { Min = 10; Max = 17; Letter = 'h' }
         Password = "ljpwchmhhzmhdhmrchp" }
       { Policy = { Min = 6; Max = 10; Letter = 'w' }
         Password = "lpcfgkslrwwrlkhx" }
       { Policy = { Min = 3; Max = 7; Letter = 'w' }
         Password = "wrpwcpw" }
       { Policy = { Min = 8; Max = 9; Letter = 'z' }
         Password = "kczxltgzh" }
       { Policy = { Min = 6; Max = 11; Letter = 'n' }
         Password = "nnnnnhnsnlnnn" }
       { Policy = { Min = 2; Max = 9; Letter = 's' }
         Password = "smssssssgss" }
       { Policy = { Min = 2; Max = 4; Letter = 'x' }
         Password = "xxwhxbfjj" }
       { Policy = { Min = 1; Max = 2; Letter = 'z' }
         Password = "fzzzzzzzzzzzzzz" }
       { Policy = { Min = 4; Max = 5; Letter = 'p' }
         Password = "pplcdpp" }
       { Policy = { Min = 3; Max = 4; Letter = 'c' }
         Password = "gncxlzc" }
       { Policy = { Min = 16; Max = 17; Letter = 'x' }
         Password = "fxqltszfgnnkxgrxhcbk" }
       { Policy = { Min = 13; Max = 17; Letter = 'n' }
         Password = "nnnnnnnnnnnnnnnnvn" }
       { Policy = { Min = 1; Max = 6; Letter = 'x' }
         Password = "dxxxxxx" }
       { Policy = { Min = 7; Max = 8; Letter = 'r' }
         Password = "scbnvqrpcbgmpmrrs" }
       { Policy = { Min = 2; Max = 17; Letter = 'd' }
         Password = "ddddddddddddddddhd" }
       { Policy = { Min = 13; Max = 19; Letter = 'v' }
         Password = "fvtphwfnmpfpbpjnnbv" }
       { Policy = { Min = 7; Max = 18; Letter = 'q' }
         Password = "cpwqnhqjqfkqqncbsh" }
       { Policy = { Min = 6; Max = 10; Letter = 'c' }
         Password = "cccdcxccncfxcgc" }
       { Policy = { Min = 2; Max = 4; Letter = 'g' }
         Password = "fggsgbgggggcggt" }
       { Policy = { Min = 13; Max = 17; Letter = 'r' }
         Password = "hspwrxrzbrvlmlwgrkxr" }
       { Policy = { Min = 14; Max = 15; Letter = 'l' }
         Password = "pllgllllllllrmv" }
       { Policy = { Min = 12; Max = 15; Letter = 'g' }
         Password = "hqgcgggsxgjxljgdz" }
       { Policy = { Min = 3; Max = 4; Letter = 'd' }
         Password = "dtxd" }
       { Policy = { Min = 7; Max = 12; Letter = 'd' }
         Password = "kddvbkkdldqbkn" }
       { Policy = { Min = 3; Max = 13; Letter = 'v' }
         Password = "vvvvvvvvvvvvtv" }
       { Policy = { Min = 8; Max = 13; Letter = 't' }
         Password = "tttttttttfttdt" }
       { Policy = { Min = 18; Max = 19; Letter = 'q' }
         Password = "hprbdznbqlfnwzwpqckb" }
       { Policy = { Min = 5; Max = 12; Letter = 'c' }
         Password = "wwlqcgzqzvtczvcldg" }
       { Policy = { Min = 3; Max = 5; Letter = 'z' }
         Password = "xzzzv" }
       { Policy = { Min = 2; Max = 11; Letter = 'c' }
         Password = "xbblzgtwcjcfqqb" }
       { Policy = { Min = 8; Max = 9; Letter = 'n' }
         Password = "nnnvbnmvl" }
       { Policy = { Min = 8; Max = 9; Letter = 'z' }
         Password = "zzzszzzzt" }
       { Policy = { Min = 2; Max = 3; Letter = 'l' }
         Password = "chsrlrl" }
       { Policy = { Min = 2; Max = 4; Letter = 'f' }
         Password = "nffm" }
       { Policy = { Min = 6; Max = 7; Letter = 'h' }
         Password = "hhhhhhhh" }
       { Policy = { Min = 10; Max = 16; Letter = 'x' }
         Password = "xxxxxxxxxxxxxxxwxxxx" }
       { Policy = { Min = 2; Max = 19; Letter = 'v' }
         Password = "ztpvktjgjlmqfrrxfpv" }
       { Policy = { Min = 2; Max = 5; Letter = 'g' }
         Password = "gncgg" }
       { Policy = { Min = 1; Max = 3; Letter = 't' }
         Password = "hjtttttvgtttttttttt" }
       { Policy = { Min = 3; Max = 4; Letter = 's' }
         Password = "nbvs" }
       { Policy = { Min = 5; Max = 10; Letter = 'n' }
         Password = "nnnqnnnnbvnnn" }
       { Policy = { Min = 7; Max = 15; Letter = 'q' }
         Password = "qqqqpqqqqqqqqqzqqsqq" }
       { Policy = { Min = 3; Max = 5; Letter = 'b' }
         Password = "sjtwbr" }
       { Policy = { Min = 2; Max = 4; Letter = 't' }
         Password = "sttxln" }
       { Policy = { Min = 1; Max = 5; Letter = 'd' }
         Password = "ddddd" }
       { Policy = { Min = 12; Max = 13; Letter = 'v' }
         Password = "zvdpfbkkvcpvdvb" }
       { Policy = { Min = 3; Max = 6; Letter = 'j' }
         Password = "cnnjhj" }
       { Policy = { Min = 7; Max = 8; Letter = 'q' }
         Password = "qqrqpbfqjvbtqlqjqkqh" }
       { Policy = { Min = 2; Max = 4; Letter = 'v' }
         Password = "wvvq" }
       { Policy = { Min = 2; Max = 7; Letter = 'm' }
         Password = "mpmrmmmmdnmmmmk" }
       { Policy = { Min = 10; Max = 14; Letter = 'g' }
         Password = "ggmcgggpggcngglm" }
       { Policy = { Min = 3; Max = 5; Letter = 'g' }
         Password = "fsbpglh" }
       { Policy = { Min = 4; Max = 5; Letter = 'r' }
         Password = "rdrtq" }
       { Policy = { Min = 3; Max = 4; Letter = 't' }
         Password = "qttltttl" }
       { Policy = { Min = 16; Max = 18; Letter = 's' }
         Password = "sssszpssbnsssssfss" }
       { Policy = { Min = 6; Max = 9; Letter = 'b' }
         Password = "lbxbwbbqn" }
       { Policy = { Min = 2; Max = 3; Letter = 'm' }
         Password = "dmwsg" }
       { Policy = { Min = 4; Max = 12; Letter = 'p' }
         Password = "lmppwmsplppx" }
       { Policy = { Min = 3; Max = 15; Letter = 'c' }
         Password = "lvjmlzwctxnckvclsj" }
       { Policy = { Min = 13; Max = 14; Letter = 't' }
         Password = "tttttttftstttw" }
       { Policy = { Min = 1; Max = 5; Letter = 'm' }
         Password = "jmmmm" }
       { Policy = { Min = 2; Max = 3; Letter = 'r' }
         Password = "rsfr" }
       { Policy = { Min = 1; Max = 4; Letter = 'd' }
         Password = "xdns" }
       { Policy = { Min = 2; Max = 3; Letter = 'k' }
         Password = "qklrwnskqnx" }
       { Policy = { Min = 1; Max = 2; Letter = 'r' }
         Password = "rrrr" }
       { Policy = { Min = 5; Max = 8; Letter = 'l' }
         Password = "vlsbftlltc" }
       { Policy = { Min = 3; Max = 12; Letter = 'n' }
         Password = "nhjlchbwphmn" }
       { Policy = { Min = 6; Max = 7; Letter = 'h' }
         Password = "thhghhv" }
       { Policy = { Min = 1; Max = 11; Letter = 'v' }
         Password = "vvvvvvsvvvk" }
       { Policy = { Min = 9; Max = 11; Letter = 'c' }
         Password = "ckdqzdkbjczkkcpdj" }
       { Policy = { Min = 7; Max = 12; Letter = 'b' }
         Password = "bbbbbjbbbbbfzbbb" }
       { Policy = { Min = 3; Max = 6; Letter = 'v' }
         Password = "vvwxkv" }
       { Policy = { Min = 6; Max = 8; Letter = 't' }
         Password = "twttttttt" }
       { Policy = { Min = 12; Max = 17; Letter = 'g' }
         Password = "gfggggggggggggggg" }
       { Policy = { Min = 2; Max = 3; Letter = 'g' }
         Password = "gqgggggggggggggg" }
       { Policy = { Min = 8; Max = 9; Letter = 'h' }
         Password = "fmjhhbjhvv" }
       { Policy = { Min = 4; Max = 7; Letter = 'q' }
         Password = "qqqqqqjsq" }
       { Policy = { Min = 4; Max = 5; Letter = 'p' }
         Password = "hpkjp" }
       { Policy = { Min = 2; Max = 10; Letter = 'h' }
         Password = "bhsgwpwnhh" }
       { Policy = { Min = 15; Max = 18; Letter = 'p' }
         Password = "nwpqxrcxgjxbbxczxb" }
       { Policy = { Min = 2; Max = 3; Letter = 'k' }
         Password = "mtkszk" }
       { Policy = { Min = 9; Max = 11; Letter = 'c' }
         Password = "zccccpccrrc" }
       { Policy = { Min = 5; Max = 6; Letter = 'c' }
         Password = "qnzjgh" }
       { Policy = { Min = 7; Max = 11; Letter = 't' }
         Password = "ttttttmtttct" }
       { Policy = { Min = 1; Max = 5; Letter = 'p' }
         Password = "pppppprplmpq" }
       { Policy = { Min = 3; Max = 4; Letter = 'x' }
         Password = "sxlc" }
       { Policy = { Min = 12; Max = 14; Letter = 'q' }
         Password = "xsqzxsrrmxvdxq" }
       { Policy = { Min = 1; Max = 3; Letter = 'k' }
         Password = "kklkjkvkkkkkk" }
       { Policy = { Min = 11; Max = 12; Letter = 'k' }
         Password = "ffflkkkkkkqkkks" }
       { Policy = { Min = 2; Max = 3; Letter = 'z' }
         Password = "zlzzz" }
       { Policy = { Min = 10; Max = 13; Letter = 'k' }
         Password = "kkxkkbkkfkckn" }
       { Policy = { Min = 11; Max = 15; Letter = 'p' }
         Password = "wkppvppxqxpnpbpkpppp" }
       { Policy = { Min = 2; Max = 11; Letter = 'r' }
         Password = "krqxlrvhwhlj" }
       { Policy = { Min = 3; Max = 4; Letter = 'l' }
         Password = "llllllrrbll" }
       { Policy = { Min = 12; Max = 14; Letter = 'n' }
         Password = "nthpvpzmwnsnnn" }
       { Policy = { Min = 15; Max = 18; Letter = 'w' }
         Password = "jwsnzwwwwwvwfdwggcw" }
       { Policy = { Min = 15; Max = 16; Letter = 'k' }
         Password = "gtxkxjvtkktkkhkr" }
       { Policy = { Min = 1; Max = 3; Letter = 'm' }
         Password = "kmzmmm" }
       { Policy = { Min = 9; Max = 10; Letter = 'j' }
         Password = "jjjjjjjjvwj" }
       { Policy = { Min = 5; Max = 8; Letter = 'p' }
         Password = "sppkrxzpbppppphpwv" }
       { Policy = { Min = 5; Max = 7; Letter = 'w' }
         Password = "wwwwgwhwwhppmqw" }
       { Policy = { Min = 5; Max = 6; Letter = 'h' }
         Password = "hchhhplrhphqq" }
       { Policy = { Min = 4; Max = 5; Letter = 'g' }
         Password = "bggbg" }
       { Policy = { Min = 3; Max = 4; Letter = 'h' }
         Password = "sbhmtvhhrbd" }
       { Policy = { Min = 1; Max = 4; Letter = 'l' }
         Password = "lqfl" }
       { Policy = { Min = 5; Max = 7; Letter = 'j' }
         Password = "jjjpjljjjj" }
       { Policy = { Min = 3; Max = 5; Letter = 'q' }
         Password = "qqqqdqqqjqqqqqqqqqqq" }
       { Policy = { Min = 1; Max = 13; Letter = 'k' }
         Password = "kkkkkkkkxkkknkk" }
       { Policy = { Min = 12; Max = 14; Letter = 'z' }
         Password = "jzzzzzzzzzzzzvz" }
       { Policy = { Min = 1; Max = 4; Letter = 'q' }
         Password = "bqqq" }
       { Policy = { Min = 8; Max = 9; Letter = 'w' }
         Password = "wkwftfmfx" }
       { Policy = { Min = 7; Max = 9; Letter = 's' }
         Password = "kssjlslpmqssx" }
       { Policy = { Min = 1; Max = 2; Letter = 'n' }
         Password = "dxzmtsvnfhjnqsfln" }
       { Policy = { Min = 15; Max = 17; Letter = 'q' }
         Password = "bqmqnrcjsmgghgqjr" }
       { Policy = { Min = 8; Max = 11; Letter = 'z' }
         Password = "zzzzzzzdzzfz" }
       { Policy = { Min = 6; Max = 7; Letter = 'z' }
         Password = "znznzzz" }
       { Policy = { Min = 8; Max = 11; Letter = 'l' }
         Password = "jvlntmjwwrrqlkzrhg" }
       { Policy = { Min = 1; Max = 5; Letter = 'r' }
         Password = "rrvrjtrrjzr" }
       { Policy = { Min = 4; Max = 20; Letter = 'd' }
         Password = "fbvprndxpfqplmtkntdd" }
       { Policy = { Min = 7; Max = 9; Letter = 'l' }
         Password = "llllllqlclllll" }
       { Policy = { Min = 3; Max = 6; Letter = 'n' }
         Password = "xrnjzmlbnjwwzdzmdj" }
       { Policy = { Min = 17; Max = 19; Letter = 'd' }
         Password = "ddddddddddddddddxddd" }
       { Policy = { Min = 9; Max = 10; Letter = 'w' }
         Password = "wwwwwckbwhww" }
       { Policy = { Min = 2; Max = 5; Letter = 'h' }
         Password = "gchshhhn" }
       { Policy = { Min = 1; Max = 4; Letter = 'l' }
         Password = "gtlq" }
       { Policy = { Min = 15; Max = 16; Letter = 'z' }
         Password = "zzzzlzzzzzzzzzzhzdzz" }
       { Policy = { Min = 5; Max = 6; Letter = 'l' }
         Password = "lllfllld" }
       { Policy = { Min = 14; Max = 16; Letter = 'j' }
         Password = "jjjjgjjjjjjjjjjjc" }
       { Policy = { Min = 6; Max = 8; Letter = 'd' }
         Password = "dddddrddd" }
       { Policy = { Min = 4; Max = 5; Letter = 'h' }
         Password = "zhshc" }
       { Policy = { Min = 8; Max = 9; Letter = 'g' }
         Password = "gmgxgbfqg" }
       { Policy = { Min = 1; Max = 8; Letter = 'r' }
         Password = "lrrrrrrzrrgrrrrr" }
       { Policy = { Min = 4; Max = 13; Letter = 'c' }
         Password = "mccqccdccccwccccccc" }
       { Policy = { Min = 3; Max = 4; Letter = 'z' }
         Password = "zhzz" }
       { Policy = { Min = 10; Max = 11; Letter = 'c' }
         Password = "crmmvznptct" }
       { Policy = { Min = 2; Max = 4; Letter = 'l' }
         Password = "slblllt" }
       { Policy = { Min = 1; Max = 6; Letter = 'q' }
         Password = "wqqdqqtqqqgdqqq" }
       { Policy = { Min = 2; Max = 13; Letter = 'l' }
         Password = "nlllpwllpjdbxvbp" }
       { Policy = { Min = 6; Max = 8; Letter = 'l' }
         Password = "mxsflqrlhkqhsrmhtwxq" }
       { Policy = { Min = 4; Max = 9; Letter = 't' }
         Password = "tpwbtdttt" }
       { Policy = { Min = 2; Max = 7; Letter = 'q' }
         Password = "fzqdrbg" }
       { Policy = { Min = 7; Max = 8; Letter = 'd' }
         Password = "ddpldttdddsd" }
       { Policy = { Min = 14; Max = 17; Letter = 'b' }
         Password = "bbbbbbbbbbbbbmbbbb" }
       { Policy = { Min = 4; Max = 11; Letter = 'x' }
         Password = "wfrxkjtpxlcbgc" }
       { Policy = { Min = 6; Max = 7; Letter = 'n' }
         Password = "nnnnnjn" }
       { Policy = { Min = 13; Max = 16; Letter = 'z' }
         Password = "zmqczdggpqzpcrlz" }
       { Policy = { Min = 1; Max = 8; Letter = 'j' }
         Password = "jjjjjjzdmjjtjj" }
       { Policy = { Min = 5; Max = 6; Letter = 'v' }
         Password = "vjsnvmb" }
       { Policy = { Min = 5; Max = 7; Letter = 'q' }
         Password = "nzqqwbqmbjwllj" }
       { Policy = { Min = 2; Max = 3; Letter = 'j' }
         Password = "mtjg" }
       { Policy = { Min = 12; Max = 15; Letter = 'd' }
         Password = "ddxdddddddddddcddd" }
       { Policy = { Min = 4; Max = 15; Letter = 'g' }
         Password = "hssvxrqgngtkcmh" }
       { Policy = { Min = 1; Max = 4; Letter = 'm' }
         Password = "mmmmmmm" }
       { Policy = { Min = 11; Max = 13; Letter = 'j' }
         Password = "jqjjjjjjjjmjj" }
       { Policy = { Min = 3; Max = 4; Letter = 'z' }
         Password = "zznzz" }
       { Policy = { Min = 2; Max = 6; Letter = 'c' }
         Password = "cccmcs" }
       { Policy = { Min = 6; Max = 10; Letter = 'x' }
         Password = "xxxxxgxxlxxpxxxx" }
       { Policy = { Min = 1; Max = 2; Letter = 'b' }
         Password = "bbrbbbbb" }
       { Policy = { Min = 2; Max = 5; Letter = 'f' }
         Password = "xfmkcf" }
       { Policy = { Min = 4; Max = 5; Letter = 'r' }
         Password = "rrrkxr" }
       { Policy = { Min = 3; Max = 4; Letter = 'z' }
         Password = "zslz" }
       { Policy = { Min = 3; Max = 4; Letter = 'w' }
         Password = "kwwh" }
       { Policy = { Min = 15; Max = 17; Letter = 'x' }
         Password = "rfxxcxwxsxsdgnxlxz" }
       { Policy = { Min = 17; Max = 18; Letter = 'w' }
         Password = "rwqlwwgwwwwjwbcjtw" }
       { Policy = { Min = 2; Max = 4; Letter = 'p' }
         Password = "ppjrpp" }
       { Policy = { Min = 16; Max = 17; Letter = 'b' }
         Password = "bbbbbbbbbbbbbbbtb" }
       { Policy = { Min = 5; Max = 6; Letter = 'b' }
         Password = "fbwbqt" }
       { Policy = { Min = 3; Max = 5; Letter = 'b' }
         Password = "bbjvxg" }
       { Policy = { Min = 4; Max = 5; Letter = 'j' }
         Password = "jbhljfjz" }
       { Policy = { Min = 4; Max = 5; Letter = 'k' }
         Password = "fmkkckpj" }
       { Policy = { Min = 18; Max = 19; Letter = 'w' }
         Password = "wpqtwhngztqkvgqrcjf" }
       { Policy = { Min = 5; Max = 6; Letter = 't' }
         Password = "wttthhtt" }
       { Policy = { Min = 12; Max = 15; Letter = 'v' }
         Password = "kvgvvvcfglsvnsp" }
       { Policy = { Min = 12; Max = 14; Letter = 'n' }
         Password = "nnnnnnnnnnnknnn" }
       { Policy = { Min = 5; Max = 8; Letter = 'k' }
         Password = "xxzhdkmmkkkbwv" }
       { Policy = { Min = 8; Max = 9; Letter = 'f' }
         Password = "fdffdgvwpfffff" }
       { Policy = { Min = 12; Max = 14; Letter = 'k' }
         Password = "kdbsqwkjhvbxrkh" }
       { Policy = { Min = 4; Max = 7; Letter = 'f' }
         Password = "fvhkstfdrwfkvv" }
       { Policy = { Min = 7; Max = 17; Letter = 'x' }
         Password = "cvkbcvbfxxgxhbxxxpbx" }
       { Policy = { Min = 11; Max = 14; Letter = 'm' }
         Password = "jjnmmmsvhzcmcm" }
       { Policy = { Min = 3; Max = 9; Letter = 'w' }
         Password = "qwxsnsxnwzsnmk" }
       { Policy = { Min = 1; Max = 5; Letter = 'k' }
         Password = "tkkkkkkkkkkkk" }
       { Policy = { Min = 5; Max = 7; Letter = 'h' }
         Password = "hhhhhhdh" }
       { Policy = { Min = 3; Max = 13; Letter = 'c' }
         Password = "cclccccccccwccccc" }
       { Policy = { Min = 1; Max = 4; Letter = 'w' }
         Password = "wwwnw" }
       { Policy = { Min = 3; Max = 7; Letter = 'z' }
         Password = "wzzblltdglmfkl" }
       { Policy = { Min = 9; Max = 12; Letter = 'k' }
         Password = "kkkwqjnqskkdhckhvkk" }
       { Policy = { Min = 2; Max = 5; Letter = 'r' }
         Password = "xjtrrsxrrdzlbjvflqxr" }
       { Policy = { Min = 9; Max = 13; Letter = 'g' }
         Password = "gggbzggggjgxkgg" }
       { Policy = { Min = 1; Max = 8; Letter = 'm' }
         Password = "zmmmmmmhmmmmmhmmmmm" }
       { Policy = { Min = 16; Max = 18; Letter = 'h' }
         Password = "hzhhhhhhhhhhhhhhhh" }
       { Policy = { Min = 2; Max = 7; Letter = 'w' }
         Password = "wwwwwwvw" }
       { Policy = { Min = 3; Max = 4; Letter = 'd' }
         Password = "ddhd" }
       { Policy = { Min = 3; Max = 5; Letter = 'x' }
         Password = "jxvzx" }
       { Policy = { Min = 15; Max = 18; Letter = 'k' }
         Password = "kkkkhkckkkkkkkkkkkxk" }
       { Policy = { Min = 11; Max = 12; Letter = 'm' }
         Password = "mmmmmmmmsmwkm" }
       { Policy = { Min = 7; Max = 8; Letter = 'k' }
         Password = "khfkkktj" }
       { Policy = { Min = 2; Max = 7; Letter = 'f' }
         Password = "ffffffff" }
       { Policy = { Min = 2; Max = 6; Letter = 'q' }
         Password = "hqqdhbfvc" }
       { Policy = { Min = 3; Max = 5; Letter = 'f' }
         Password = "rlpffgf" }
       { Policy = { Min = 3; Max = 4; Letter = 't' }
         Password = "wtltht" }
       { Policy = { Min = 4; Max = 5; Letter = 'f' }
         Password = "fscfx" }
       { Policy = { Min = 2; Max = 16; Letter = 't' }
         Password = "nmtppmqttqztvdstc" }
       { Policy = { Min = 1; Max = 15; Letter = 'j' }
         Password = "jwgcbkdjlmjjxzwvpvd" }
       { Policy = { Min = 10; Max = 12; Letter = 'v' }
         Password = "vvhvfvvqvvvv" }
       { Policy = { Min = 5; Max = 6; Letter = 'l' }
         Password = "llllbwlll" }
       { Policy = { Min = 1; Max = 2; Letter = 'z' }
         Password = "xmszvzrwpm" }
       { Policy = { Min = 6; Max = 11; Letter = 'd' }
         Password = "dddjndddddq" }
       { Policy = { Min = 4; Max = 9; Letter = 'r' }
         Password = "xwkfwcztcq" }
       { Policy = { Min = 9; Max = 10; Letter = 'k' }
         Password = "ckskkkktkr" }
       { Policy = { Min = 2; Max = 4; Letter = 'x' }
         Password = "txpxfq" }
       { Policy = { Min = 1; Max = 3; Letter = 'j' }
         Password = "sjzj" }
       { Policy = { Min = 7; Max = 11; Letter = 'x' }
         Password = "bbhcswxtnhx" }
       { Policy = { Min = 9; Max = 10; Letter = 'q' }
         Password = "jlqnqmhjqhqq" }
       { Policy = { Min = 4; Max = 19; Letter = 'd' }
         Password = "qddkdmptbvjpbrjdzddl" }
       { Policy = { Min = 7; Max = 9; Letter = 'd' }
         Password = "sqdpdhhdx" }
       { Policy = { Min = 7; Max = 8; Letter = 'j' }
         Password = "gjzmzjgd" }
       { Policy = { Min = 10; Max = 15; Letter = 's' }
         Password = "gkgsssssssqssssrpc" }
       { Policy = { Min = 5; Max = 6; Letter = 'v' }
         Password = "vvvvhbvh" }
       { Policy = { Min = 1; Max = 3; Letter = 'c' }
         Password = "cccc" }
       { Policy = { Min = 1; Max = 3; Letter = 'c' }
         Password = "ccwcccczgccpccz" }
       { Policy = { Min = 2; Max = 4; Letter = 't' }
         Password = "tgtmqtl" }
       { Policy = { Min = 11; Max = 13; Letter = 'w' }
         Password = "wwwcwwwwwwlhw" }
       { Policy = { Min = 4; Max = 5; Letter = 'z' }
         Password = "nzgzrz" }
       { Policy = { Min = 4; Max = 11; Letter = 's' }
         Password = "lhzxmwclxss" }
       { Policy = { Min = 15; Max = 18; Letter = 's' }
         Password = "hmszwkscbdzsrgssjj" }
       { Policy = { Min = 4; Max = 5; Letter = 'm' }
         Password = "wkvgzjmhxmwlmlmvsjv" }
       { Policy = { Min = 11; Max = 12; Letter = 't' }
         Password = "lndqtmsfwpjp" }
       { Policy = { Min = 2; Max = 10; Letter = 'w' }
         Password = "wkwwwwwwwww" }
       { Policy = { Min = 10; Max = 11; Letter = 't' }
         Password = "ttgpwkjltgn" }
       { Policy = { Min = 3; Max = 9; Letter = 'b' }
         Password = "bbvbbbbbtb" }
       { Policy = { Min = 5; Max = 7; Letter = 'h' }
         Password = "rqlbntrhhkjhhhrdhq" }
       { Policy = { Min = 1; Max = 2; Letter = 'n' }
         Password = "rnnrbnn" }
       { Policy = { Min = 8; Max = 11; Letter = 'n' }
         Password = "nnnnnnnpnnnnnn" }
       { Policy = { Min = 4; Max = 5; Letter = 's' }
         Password = "vhsnsjc" }
       { Policy = { Min = 5; Max = 7; Letter = 'b' }
         Password = "tbbbbbcbb" }
       { Policy = { Min = 1; Max = 3; Letter = 'q' }
         Password = "frbq" }
       { Policy = { Min = 3; Max = 4; Letter = 's' }
         Password = "xsssmfsgs" }
       { Policy = { Min = 13; Max = 17; Letter = 'k' }
         Password = "kkkkkbfkkkkkvkkkkkkk" }
       { Policy = { Min = 1; Max = 13; Letter = 'v' }
         Password = "zvvvvvvvvvvvvv" }
       { Policy = { Min = 11; Max = 14; Letter = 'c' }
         Password = "cbcmcccccccmccc" }
       { Policy = { Min = 15; Max = 17; Letter = 'r' }
         Password = "skkrrvsrlmrrrrrjdrrr" }
       { Policy = { Min = 1; Max = 7; Letter = 'm' }
         Password = "jmmqmmmmkmmmrkmmr" }
       { Policy = { Min = 9; Max = 14; Letter = 'f' }
         Password = "kstfsxflhffxsffkb" }
       { Policy = { Min = 7; Max = 9; Letter = 'g' }
         Password = "ggggggggvggggg" }
       { Policy = { Min = 13; Max = 16; Letter = 't' }
         Password = "tttttttgtttttttvtt" }
       { Policy = { Min = 9; Max = 10; Letter = 'p' }
         Password = "ppppppppphp" }
       { Policy = { Min = 3; Max = 4; Letter = 'w' }
         Password = "wwxw" }
       { Policy = { Min = 9; Max = 13; Letter = 'g' }
         Password = "ggggggggrgggvg" }
       { Policy = { Min = 3; Max = 4; Letter = 'f' }
         Password = "ffkffq" }
       { Policy = { Min = 8; Max = 11; Letter = 'h' }
         Password = "hbhhzhhhhhfh" }
       { Policy = { Min = 2; Max = 4; Letter = 'd' }
         Password = "dcnss" }
       { Policy = { Min = 6; Max = 7; Letter = 'r' }
         Password = "rtrrrbr" }
       { Policy = { Min = 5; Max = 6; Letter = 'r' }
         Password = "rrrrxq" }
       { Policy = { Min = 1; Max = 11; Letter = 'g' }
         Password = "fgggggggmkglk" }
       { Policy = { Min = 14; Max = 15; Letter = 'h' }
         Password = "vlqkqhhhfwhxfvs" }
       { Policy = { Min = 3; Max = 4; Letter = 'w' }
         Password = "wlrsgfsw" }
       { Policy = { Min = 1; Max = 2; Letter = 'v' }
         Password = "dxkwzvvxv" }
       { Policy = { Min = 2; Max = 4; Letter = 'r' }
         Password = "rvrcrtrrl" }
       { Policy = { Min = 4; Max = 6; Letter = 't' }
         Password = "ttktttt" }
       { Policy = { Min = 10; Max = 15; Letter = 'j' }
         Password = "jjjjbtjjtjnjjjk" }
       { Policy = { Min = 5; Max = 6; Letter = 's' }
         Password = "ssssssss" }
       { Policy = { Min = 5; Max = 7; Letter = 's' }
         Password = "sfnkzss" }
       { Policy = { Min = 4; Max = 5; Letter = 'b' }
         Password = "shbtb" }
       { Policy = { Min = 2; Max = 5; Letter = 'j' }
         Password = "hjktjm" }
       { Policy = { Min = 1; Max = 5; Letter = 'h' }
         Password = "hhhhdhhhhh" }
       { Policy = { Min = 5; Max = 17; Letter = 'm' }
         Password = "mmmmgmmmmmmmmmmmmrmn" }
       { Policy = { Min = 2; Max = 6; Letter = 'b' }
         Password = "cxgxbbskzgdhr" }
       { Policy = { Min = 10; Max = 12; Letter = 'k' }
         Password = "kkkkkkkkkbkkkkknkmks" }
       { Policy = { Min = 13; Max = 16; Letter = 'g' }
         Password = "ggggggqggggghggggggg" }
       { Policy = { Min = 1; Max = 2; Letter = 'w' }
         Password = "wwwl" }
       { Policy = { Min = 6; Max = 9; Letter = 'b' }
         Password = "bkbbmbbbzb" }
       { Policy = { Min = 6; Max = 7; Letter = 'm' }
         Password = "qrfhmmndrkmc" }
       { Policy = { Min = 5; Max = 11; Letter = 'p' }
         Password = "ggzmjkxpnrpf" }
       { Policy = { Min = 2; Max = 3; Letter = 'r' }
         Password = "rhrr" }
       { Policy = { Min = 6; Max = 7; Letter = 'f' }
         Password = "vppvpwf" }
       { Policy = { Min = 8; Max = 10; Letter = 'w' }
         Password = "wrwwwdvwwjwwww" }
       { Policy = { Min = 6; Max = 11; Letter = 'c' }
         Password = "wxrbztwpcccj" }
       { Policy = { Min = 14; Max = 17; Letter = 'x' }
         Password = "xxxxxxxxxxxxxrxxxxxx" }
       { Policy = { Min = 5; Max = 8; Letter = 'c' }
         Password = "cccccczqccc" }
       { Policy = { Min = 2; Max = 6; Letter = 'j' }
         Password = "jgqjjfjzjjjjjjmjjj" }
       { Policy = { Min = 4; Max = 7; Letter = 't' }
         Password = "zphkzttgtjdxdtd" }
       { Policy = { Min = 4; Max = 7; Letter = 't' }
         Password = "wsrtdqgthqjvznbj" }
       { Policy = { Min = 15; Max = 19; Letter = 'h' }
         Password = "hmhhhhzhhhchhmhhhtxh" }
       { Policy = { Min = 1; Max = 3; Letter = 'z' }
         Password = "zzzz" }
       { Policy = { Min = 2; Max = 3; Letter = 'j' }
         Password = "jcvl" }
       { Policy = { Min = 1; Max = 7; Letter = 'w' }
         Password = "wcpwswwgjfb" }
       { Policy = { Min = 3; Max = 6; Letter = 'c' }
         Password = "crsvmcckc" }
       { Policy = { Min = 9; Max = 10; Letter = 'f' }
         Password = "fffffffffjff" }
       { Policy = { Min = 3; Max = 6; Letter = 'v' }
         Password = "hfvpwvgg" }
       { Policy = { Min = 2; Max = 5; Letter = 'r' }
         Password = "dkhrrd" }
       { Policy = { Min = 1; Max = 5; Letter = 'f' }
         Password = "cflmflfdvbz" }
       { Policy = { Min = 3; Max = 13; Letter = 'k' }
         Password = "sfkgcgktfkhrh" }
       { Policy = { Min = 3; Max = 9; Letter = 'v' }
         Password = "mmrprsvzv" }
       { Policy = { Min = 3; Max = 4; Letter = 'q' }
         Password = "qqqbcrkq" }
       { Policy = { Min = 11; Max = 13; Letter = 'r' }
         Password = "rrrwrrrrrrrgrr" }
       { Policy = { Min = 6; Max = 11; Letter = 'j' }
         Password = "tjjjzpsjrjdj" }
       { Policy = { Min = 14; Max = 18; Letter = 't' }
         Password = "dtbhmtltcwpnzwqtgt" }
       { Policy = { Min = 2; Max = 5; Letter = 'c' }
         Password = "rsccchcc" }
       { Policy = { Min = 11; Max = 14; Letter = 'm' }
         Password = "kmmmmmlvmmtmmm" }
       { Policy = { Min = 7; Max = 10; Letter = 'x' }
         Password = "xhxxxxxbxbhxxxx" }
       { Policy = { Min = 10; Max = 13; Letter = 'n' }
         Password = "nnntnnnnnpnnn" }
       { Policy = { Min = 3; Max = 10; Letter = 'w' }
         Password = "wwwjwgwwwgwmww" }
       { Policy = { Min = 17; Max = 18; Letter = 'p' }
         Password = "phpppnpqppjsrpppzj" }
       { Policy = { Min = 8; Max = 12; Letter = 'r' }
         Password = "rsrbwrrrrrrzr" }
       { Policy = { Min = 9; Max = 15; Letter = 'q' }
         Password = "bqlrdqqxrdqqnxq" }
       { Policy = { Min = 5; Max = 11; Letter = 'd' }
         Password = "sldcndtlpzdb" }
       { Policy = { Min = 1; Max = 3; Letter = 'w' }
         Password = "zwww" }
       { Policy = { Min = 11; Max = 12; Letter = 'k' }
         Password = "tkbkwkkvsblpt" }
       { Policy = { Min = 13; Max = 14; Letter = 'c' }
         Password = "ccccccccccccqc" }
       { Policy = { Min = 1; Max = 5; Letter = 'c' }
         Password = "ccccrc" }
       { Policy = { Min = 4; Max = 5; Letter = 'f' }
         Password = "fffnf" }
       { Policy = { Min = 3; Max = 4; Letter = 'w' }
         Password = "wwwvw" }
       { Policy = { Min = 2; Max = 4; Letter = 'k' }
         Password = "kzkk" }
       { Policy = { Min = 16; Max = 18; Letter = 'j' }
         Password = "jjjjjjjjjjjjjjvqjj" }
       { Policy = { Min = 2; Max = 8; Letter = 'v' }
         Password = "wvqlrnrtgbzrp" }
       { Policy = { Min = 6; Max = 10; Letter = 'c' }
         Password = "cccccdcccccc" }
       { Policy = { Min = 1; Max = 4; Letter = 'q' }
         Password = "bqqqq" }
       { Policy = { Min = 5; Max = 6; Letter = 'n' }
         Password = "nnnnnnn" }
       { Policy = { Min = 2; Max = 16; Letter = 'f' }
         Password = "cjrffhfpfflxljjfp" }
       { Policy = { Min = 3; Max = 8; Letter = 'g' }
         Password = "ggfggggggg" }
       { Policy = { Min = 7; Max = 8; Letter = 'z' }
         Password = "zmzkzzzczwzzzz" }
       { Policy = { Min = 7; Max = 8; Letter = 'm' }
         Password = "mmmmmmmmmm" }
       { Policy = { Min = 7; Max = 9; Letter = 'f' }
         Password = "vzlffftfw" }
       { Policy = { Min = 4; Max = 10; Letter = 'w' }
         Password = "kckwgbmtws" }
       { Policy = { Min = 4; Max = 5; Letter = 'g' }
         Password = "ggghgp" }
       { Policy = { Min = 6; Max = 17; Letter = 'w' }
         Password = "wwwwwwwwwwwwwwwwkw" }
       { Policy = { Min = 3; Max = 16; Letter = 'f' }
         Password = "fffbfffffffffffcff" }
       { Policy = { Min = 9; Max = 14; Letter = 'l' }
         Password = "lllllllwmllmblllhlml" }
       { Policy = { Min = 1; Max = 4; Letter = 's' }
         Password = "sssdssss" }
       { Policy = { Min = 3; Max = 4; Letter = 'm' }
         Password = "lmnm" }
       { Policy = { Min = 10; Max = 11; Letter = 'v' }
         Password = "vvvvkvsvvvmvhv" }
       { Policy = { Min = 3; Max = 4; Letter = 'p' }
         Password = "pprb" }
       { Policy = { Min = 3; Max = 4; Letter = 'k' }
         Password = "pkqk" }
       { Policy = { Min = 3; Max = 4; Letter = 'd' }
         Password = "ddxd" }
       { Policy = { Min = 7; Max = 8; Letter = 'b' }
         Password = "bbbbbbfb" }
       { Policy = { Min = 5; Max = 7; Letter = 'w' }
         Password = "qbmhsmt" }
       { Policy = { Min = 11; Max = 12; Letter = 'b' }
         Password = "bbbbbbbbbbbgb" }
       { Policy = { Min = 3; Max = 5; Letter = 'x' }
         Password = "xpxbljxt" }
       { Policy = { Min = 2; Max = 9; Letter = 'z' }
         Password = "kzmpqtbvzrqzh" }
       { Policy = { Min = 3; Max = 16; Letter = 'v' }
         Password = "qwvfvltjrpdxmvqv" }
       { Policy = { Min = 2; Max = 6; Letter = 'n' }
         Password = "pdjxzkn" }
       { Policy = { Min = 7; Max = 8; Letter = 'j' }
         Password = "jmzvjkjk" }
       { Policy = { Min = 2; Max = 5; Letter = 'r' }
         Password = "rrfjqqft" }
       { Policy = { Min = 2; Max = 5; Letter = 'h' }
         Password = "pwhfh" }
       { Policy = { Min = 6; Max = 7; Letter = 'm' }
         Password = "mmgvjmm" }
       { Policy = { Min = 11; Max = 12; Letter = 'r' }
         Password = "rrrrrrrrrrxqrr" }
       { Policy = { Min = 1; Max = 4; Letter = 'n' }
         Password = "nnnw" }
       { Policy = { Min = 1; Max = 5; Letter = 'z' }
         Password = "szzzzzdtzz" }
       { Policy = { Min = 7; Max = 13; Letter = 'j' }
         Password = "jjjjjjnjjjjjbj" }
       { Policy = { Min = 10; Max = 15; Letter = 'w' }
         Password = "rwwwwtmwswwwwwwwnmbk" }
       { Policy = { Min = 11; Max = 13; Letter = 't' }
         Password = "twxhrldqtttmnt" }
       { Policy = { Min = 1; Max = 2; Letter = 'r' }
         Password = "bkbbrwr" }
       { Policy = { Min = 11; Max = 17; Letter = 'h' }
         Password = "hhdhhhhhhshqpbhhn" }
       { Policy = { Min = 4; Max = 7; Letter = 'c' }
         Password = "crgchccbnr" }
       { Policy = { Min = 9; Max = 11; Letter = 'r' }
         Password = "bdhgrzkmrrl" }
       { Policy = { Min = 6; Max = 8; Letter = 'g' }
         Password = "gggggggzz" }
       { Policy = { Min = 3; Max = 9; Letter = 'g' }
         Password = "ggggggggqg" }
       { Policy = { Min = 9; Max = 11; Letter = 'z' }
         Password = "zrfcqtrxxqzcx" }
       { Policy = { Min = 3; Max = 9; Letter = 's' }
         Password = "zstjqhnvgjjfxknt" }
       { Policy = { Min = 12; Max = 13; Letter = 'p' }
         Password = "pppppwpgcppjppppptp" }
       { Policy = { Min = 6; Max = 7; Letter = 'k' }
         Password = "kkwrkckb" }
       { Policy = { Min = 8; Max = 9; Letter = 'k' }
         Password = "kkkkqzjkn" }
       { Policy = { Min = 8; Max = 9; Letter = 'l' }
         Password = "lrxlkbflrl" }
       { Policy = { Min = 1; Max = 3; Letter = 'n' }
         Password = "nndn" }
       { Policy = { Min = 8; Max = 9; Letter = 'd' }
         Password = "ddhddddddd" }
       { Policy = { Min = 4; Max = 12; Letter = 'g' }
         Password = "zdclfqvdgnzfv" }
       { Policy = { Min = 3; Max = 5; Letter = 'd' }
         Password = "ddddkddddddd" }
       { Policy = { Min = 9; Max = 11; Letter = 'x' }
         Password = "xxxxxxxxqxxx" }
       { Policy = { Min = 4; Max = 7; Letter = 't' }
         Password = "ttttfftt" }
       { Policy = { Min = 2; Max = 4; Letter = 'n' }
         Password = "wfmnnddqxfm" }
       { Policy = { Min = 16; Max = 19; Letter = 'r' }
         Password = "zhjsgxjkjpqmpvkrjgr" }
       { Policy = { Min = 3; Max = 7; Letter = 'v' }
         Password = "vvfvvvvv" }
       { Policy = { Min = 1; Max = 2; Letter = 'd' }
         Password = "qdwdfj" }
       { Policy = { Min = 6; Max = 10; Letter = 'h' }
         Password = "hhhhhhhhhrhh" }
       { Policy = { Min = 4; Max = 16; Letter = 'x' }
         Password = "xxxpxxxxxxxxxxxxx" }
       { Policy = { Min = 18; Max = 19; Letter = 'q' }
         Password = "qqqqqqqqqqqqqlqqqqf" }
       { Policy = { Min = 6; Max = 10; Letter = 'g' }
         Password = "gkcntgbgbggklsx" }
       { Policy = { Min = 8; Max = 9; Letter = 'n' }
         Password = "nnnnxnnnpnn" }
       { Policy = { Min = 7; Max = 9; Letter = 'm' }
         Password = "msmmmtdvm" }
       { Policy = { Min = 2; Max = 15; Letter = 'd' }
         Password = "twjdrfzntqhnwkd" }
       { Policy = { Min = 1; Max = 4; Letter = 'z' }
         Password = "kzzz" }
       { Policy = { Min = 16; Max = 18; Letter = 'b' }
         Password = "tbbbtbjbtbtflzckhb" }
       { Policy = { Min = 4; Max = 12; Letter = 'k' }
         Password = "kkbhkgkrkgfk" }
       { Policy = { Min = 8; Max = 10; Letter = 'q' }
         Password = "lrqrjqvwmrb" }
       { Policy = { Min = 1; Max = 3; Letter = 'f' }
         Password = "vfhf" }
       { Policy = { Min = 7; Max = 14; Letter = 'v' }
         Password = "vvvvvvrtvvvvvvvv" }
       { Policy = { Min = 4; Max = 5; Letter = 'n' }
         Password = "xnntnwntrfnbqqdk" }
       { Policy = { Min = 3; Max = 5; Letter = 'r' }
         Password = "rhkrzwrhrrr" }
       { Policy = { Min = 2; Max = 4; Letter = 'b' }
         Password = "bspbjb" }
       { Policy = { Min = 5; Max = 6; Letter = 's' }
         Password = "sfscsc" }
       { Policy = { Min = 6; Max = 7; Letter = 'x' }
         Password = "xxxxxhx" }
       { Policy = { Min = 8; Max = 10; Letter = 'w' }
         Password = "wwwwbzlmqw" }
       { Policy = { Min = 7; Max = 10; Letter = 'v' }
         Password = "fkvdvjbfvd" }
       { Policy = { Min = 2; Max = 5; Letter = 'q' }
         Password = "qtqspqqq" }
       { Policy = { Min = 8; Max = 9; Letter = 'k' }
         Password = "kmhkkhpsk" }
       { Policy = { Min = 5; Max = 8; Letter = 'h' }
         Password = "xhdhjfph" }
       { Policy = { Min = 3; Max = 6; Letter = 'b' }
         Password = "dlbkbb" }
       { Policy = { Min = 1; Max = 3; Letter = 'w' }
         Password = "wwbswwww" }
       { Policy = { Min = 2; Max = 4; Letter = 'x' }
         Password = "mxtx" }
       { Policy = { Min = 2; Max = 4; Letter = 'l' }
         Password = "llrll" }
       { Policy = { Min = 3; Max = 7; Letter = 'j' }
         Password = "kclqzgc" }
       { Policy = { Min = 2; Max = 3; Letter = 'r' }
         Password = "rxrrrgrrrrr" }
       { Policy = { Min = 2; Max = 4; Letter = 'q' }
         Password = "nzwxlmcqqqm" }
       { Policy = { Min = 15; Max = 16; Letter = 'h' }
         Password = "hhhhvmhbhdtbblbh" }
       { Policy = { Min = 13; Max = 19; Letter = 'l' }
         Password = "ltkftclmlllflzltlnb" }
       { Policy = { Min = 4; Max = 5; Letter = 'p' }
         Password = "zmwtpjrltqdmfppz" }
       { Policy = { Min = 6; Max = 10; Letter = 't' }
         Password = "tjdxqtsbzhvprspljmv" }
       { Policy = { Min = 14; Max = 17; Letter = 'q' }
         Password = "qcqqqqqcqghqqqqqjq" }
       { Policy = { Min = 1; Max = 5; Letter = 'j' }
         Password = "flxrjspwlrdqsnjcs" }
       { Policy = { Min = 14; Max = 15; Letter = 'm' }
         Password = "mmmlmmmmmmmmmwm" }
       { Policy = { Min = 3; Max = 5; Letter = 'd' }
         Password = "dddvkwksdcrktlpd" }
       { Policy = { Min = 8; Max = 11; Letter = 'l' }
         Password = "llcllllxllml" }
       { Policy = { Min = 2; Max = 4; Letter = 'v' }
         Password = "vvvbv" }
       { Policy = { Min = 1; Max = 3; Letter = 'g' }
         Password = "llggz" }
       { Policy = { Min = 3; Max = 5; Letter = 'q' }
         Password = "znqqmt" }
       { Policy = { Min = 15; Max = 17; Letter = 'f' }
         Password = "ffffffffffffffjfff" }
       { Policy = { Min = 17; Max = 18; Letter = 'q' }
         Password = "zwnkmcqdqlqgkwfmqc" }
       { Policy = { Min = 8; Max = 11; Letter = 'f' }
         Password = "fffsrffbfffffvfxf" }
       { Policy = { Min = 1; Max = 7; Letter = 'b' }
         Password = "bbbbbbbb" }
       { Policy = { Min = 3; Max = 4; Letter = 'l' }
         Password = "llzh" }
       { Policy = { Min = 8; Max = 9; Letter = 'n' }
         Password = "nhnnnnqknnbnncncnnl" }
       { Policy = { Min = 9; Max = 11; Letter = 'v' }
         Password = "wvvvvvvbhjc" }
       { Policy = { Min = 15; Max = 16; Letter = 'q' }
         Password = "qcjqvfdcsqwdrqqt" }
       { Policy = { Min = 9; Max = 10; Letter = 'j' }
         Password = "jcckdzkzjjb" }
       { Policy = { Min = 1; Max = 2; Letter = 's' }
         Password = "hssmsssms" }
       { Policy = { Min = 1; Max = 3; Letter = 'w' }
         Password = "xwww" }
       { Policy = { Min = 2; Max = 4; Letter = 'l' }
         Password = "lllll" }
       { Policy = { Min = 2; Max = 4; Letter = 'q' }
         Password = "qnmq" }
       { Policy = { Min = 16; Max = 18; Letter = 't' }
         Password = "tttttttgtftttttttt" }
       { Policy = { Min = 5; Max = 6; Letter = 't' }
         Password = "kttttj" }
       { Policy = { Min = 16; Max = 17; Letter = 't' }
         Password = "twlqttttttttttmct" }
       { Policy = { Min = 8; Max = 15; Letter = 'x' }
         Password = "xxwpxsqkxgkxgxxbdgx" }
       { Policy = { Min = 17; Max = 18; Letter = 'h' }
         Password = "hhhhhhhhhhblhhhhrq" }
       { Policy = { Min = 12; Max = 17; Letter = 'm' }
         Password = "fmkmmmmqkmmdrbvthm" }
       { Policy = { Min = 2; Max = 4; Letter = 'b' }
         Password = "fbcb" }
       { Policy = { Min = 1; Max = 14; Letter = 't' }
         Password = "tttttttttttttqtt" }
       { Policy = { Min = 17; Max = 18; Letter = 'v' }
         Password = "vvvvvvvvvvvvvvvvvnv" }
       { Policy = { Min = 7; Max = 10; Letter = 'x' }
         Password = "vxxtxlxxlk" }
       { Policy = { Min = 3; Max = 5; Letter = 'n' }
         Password = "nnmnqnnb" }
       { Policy = { Min = 2; Max = 8; Letter = 's' }
         Password = "vssjqsssssb" }
       { Policy = { Min = 9; Max = 11; Letter = 'l' }
         Password = "wlllllllllllll" }
       { Policy = { Min = 4; Max = 14; Letter = 'r' }
         Password = "zrlcrxrrrzrrrrr" }
       { Policy = { Min = 3; Max = 14; Letter = 'n' }
         Password = "wrnjpnkndsshqk" }
       { Policy = { Min = 12; Max = 16; Letter = 'p' }
         Password = "ppppzpppppphppppp" }
       { Policy = { Min = 9; Max = 12; Letter = 'r' }
         Password = "rrrrrcbrrfprrrrr" }
       { Policy = { Min = 2; Max = 3; Letter = 'b' }
         Password = "bbrb" }
       { Policy = { Min = 14; Max = 16; Letter = 'd' }
         Password = "tzdjdndddgsddlnddgd" }
       { Policy = { Min = 16; Max = 18; Letter = 'c' }
         Password = "cccccccccccccccwcc" }
       { Policy = { Min = 5; Max = 6; Letter = 'v' }
         Password = "rvvqvt" }
       { Policy = { Min = 11; Max = 17; Letter = 's' }
         Password = "ssssssssssssssssps" }
       { Policy = { Min = 8; Max = 9; Letter = 'v' }
         Password = "vpvxqvvdvnvhgnvvlvs" }
       { Policy = { Min = 7; Max = 8; Letter = 'd' }
         Password = "ddddqlrt" }
       { Policy = { Min = 7; Max = 13; Letter = 'd' }
         Password = "bfzrkddtdwqld" }
       { Policy = { Min = 4; Max = 6; Letter = 'c' }
         Password = "cccccq" }
       { Policy = { Min = 6; Max = 8; Letter = 'd' }
         Password = "hkdndlqq" }
       { Policy = { Min = 11; Max = 13; Letter = 'l' }
         Password = "ngmllbdklvlmqlz" }
       { Policy = { Min = 8; Max = 17; Letter = 'm' }
         Password = "mmmmfmmmmmmmmmmmlmm" }
       { Policy = { Min = 12; Max = 15; Letter = 'b' }
         Password = "bbsbbcblbsnbzbbfcfzz" }
       { Policy = { Min = 12; Max = 13; Letter = 'k' }
         Password = "gbwkkkkkkkksk" }
       { Policy = { Min = 12; Max = 14; Letter = 'x' }
         Password = "xxxxwxxxxxxdxxxxxxx" }
       { Policy = { Min = 3; Max = 4; Letter = 'm' }
         Password = "mwsmp" }
       { Policy = { Min = 5; Max = 6; Letter = 'k' }
         Password = "kkkkzk" }
       { Policy = { Min = 4; Max = 5; Letter = 'h' }
         Password = "pqslhh" }
       { Policy = { Min = 7; Max = 13; Letter = 'l' }
         Password = "gmpxpvwqrnlfp" }
       { Policy = { Min = 3; Max = 6; Letter = 't' }
         Password = "sttxtmtn" }
       { Policy = { Min = 11; Max = 13; Letter = 'r' }
         Password = "rrbmbrwrrrrrkhrr" }
       { Policy = { Min = 14; Max = 16; Letter = 's' }
         Password = "ssssssssssssstsss" }
       { Policy = { Min = 7; Max = 10; Letter = 'v' }
         Password = "vvvvvvhvdvvvkv" }
       { Policy = { Min = 5; Max = 6; Letter = 'z' }
         Password = "sxpzzx" }
       { Policy = { Min = 2; Max = 4; Letter = 'd' }
         Password = "rmxd" }
       { Policy = { Min = 16; Max = 17; Letter = 'z' }
         Password = "zzzzzzzzzzzzzzzzzz" }
       { Policy = { Min = 1; Max = 3; Letter = 'k' }
         Password = "kjkkkkcckkzk" }
       { Policy = { Min = 1; Max = 11; Letter = 'k' }
         Password = "xzkkkkzkppk" }
       { Policy = { Min = 8; Max = 9; Letter = 'f' }
         Password = "bfvfdffzb" }
       { Policy = { Min = 4; Max = 14; Letter = 'r' }
         Password = "rfzcrrlmxqlrrrqr" }
       { Policy = { Min = 7; Max = 19; Letter = 't' }
         Password = "gtnxjqtnjbkrwpzshqqn" }
       { Policy = { Min = 2; Max = 5; Letter = 'j' }
         Password = "kjjgpddjpjjjffzjjp" }
       { Policy = { Min = 2; Max = 3; Letter = 'f' }
         Password = "cfffh" }
       { Policy = { Min = 1; Max = 2; Letter = 'x' }
         Password = "xxxxx" }
       { Policy = { Min = 3; Max = 13; Letter = 'j' }
         Password = "jjjjjjjjjjjjzfjjj" }
       { Policy = { Min = 7; Max = 8; Letter = 'm' }
         Password = "sgmmpmjmwmmmtfs" }
       { Policy = { Min = 4; Max = 12; Letter = 'z' }
         Password = "zfzqzzszvtml" }
       { Policy = { Min = 6; Max = 9; Letter = 'b' }
         Password = "jsfbpkzwb" }
       { Policy = { Min = 13; Max = 16; Letter = 'x' }
         Password = "zsxxjxxsxxqxpxxx" }
       { Policy = { Min = 8; Max = 12; Letter = 'b' }
         Password = "rlzdlplbgbdgd" }
       { Policy = { Min = 3; Max = 14; Letter = 'h' }
         Password = "hmrhhhhhhhhhrthhhh" }
       { Policy = { Min = 15; Max = 19; Letter = 'g' }
         Password = "mgggggcgggggqgghggg" }
       { Policy = { Min = 2; Max = 9; Letter = 'p' }
         Password = "ppppptppzcf" }
       { Policy = { Min = 6; Max = 7; Letter = 'b' }
         Password = "bbbbbbbbb" }
       { Policy = { Min = 4; Max = 20; Letter = 'q' }
         Password = "skqqvxptdswwnrflkvxq" }
       { Policy = { Min = 4; Max = 5; Letter = 't' }
         Password = "lqttq" }
       { Policy = { Min = 1; Max = 10; Letter = 'l' }
         Password = "lqkqllvllj" }
       { Policy = { Min = 11; Max = 15; Letter = 'm' }
         Password = "qmmmmmrmqmmmsmf" }
       { Policy = { Min = 6; Max = 15; Letter = 's' }
         Password = "ssssstssssssssss" }
       { Policy = { Min = 2; Max = 4; Letter = 'x' }
         Password = "xtxxx" }
       { Policy = { Min = 9; Max = 11; Letter = 'q' }
         Password = "qqqqqqqqhqgqq" }
       { Policy = { Min = 1; Max = 4; Letter = 'n' }
         Password = "gpnnfnn" }
       { Policy = { Min = 1; Max = 3; Letter = 'l' }
         Password = "lltl" }
       { Policy = { Min = 11; Max = 15; Letter = 'k' }
         Password = "kkfkkfkmmkrkkkk" }
       { Policy = { Min = 11; Max = 12; Letter = 'f' }
         Password = "fkcvfvtqfcfffffffffj" }
       { Policy = { Min = 1; Max = 4; Letter = 'c' }
         Password = "ccjc" }
       { Policy = { Min = 14; Max = 15; Letter = 'n' }
         Password = "bvbvfvzcbfnzqlsvh" }
       { Policy = { Min = 4; Max = 5; Letter = 'x' }
         Password = "xxlmxx" }
       { Policy = { Min = 3; Max = 6; Letter = 'n' }
         Password = "nnrnnnwlnncnn" }
       { Policy = { Min = 6; Max = 9; Letter = 'j' }
         Password = "jjjjjjjjq" }
       { Policy = { Min = 7; Max = 10; Letter = 'd' }
         Password = "pdplmxdczddbd" }
       { Policy = { Min = 12; Max = 13; Letter = 'c' }
         Password = "ccccbctccccccccc" }
       { Policy = { Min = 12; Max = 13; Letter = 'j' }
         Password = "jfjdjjjjjjjjjj" }
       { Policy = { Min = 6; Max = 7; Letter = 'h' }
         Password = "mrnphwh" }
       { Policy = { Min = 2; Max = 9; Letter = 'n' }
         Password = "njnnnnnnnnnnnn" }
       { Policy = { Min = 3; Max = 6; Letter = 'g' }
         Password = "rgxgggggnjghgggntg" }
       { Policy = { Min = 9; Max = 12; Letter = 'b' }
         Password = "bbbbbbbbbbbcbb" }
       { Policy = { Min = 3; Max = 5; Letter = 'p' }
         Password = "ppppvpp" }
       { Policy = { Min = 16; Max = 20; Letter = 't' }
         Password = "ctkgpgzrwwngltvxcqct" }
       { Policy = { Min = 4; Max = 5; Letter = 's' }
         Password = "sssdsh" }
       { Policy = { Min = 12; Max = 14; Letter = 'v' }
         Password = "vvvvvsvvvvvvvsv" }
       { Policy = { Min = 8; Max = 13; Letter = 'w' }
         Password = "zwwwwwwvwwrwgv" }
       { Policy = { Min = 12; Max = 17; Letter = 'r' }
         Password = "wrcrrrrrrrbrwrrrxr" }
       { Policy = { Min = 12; Max = 13; Letter = 'x' }
         Password = "xxxxxxxxxjsvrnxx" }
       { Policy = { Min = 7; Max = 9; Letter = 'n' }
         Password = "nqnnqnvnn" }
       { Policy = { Min = 14; Max = 19; Letter = 'n' }
         Password = "nnnnnnnnnnnnnnnnnnnn" }
       { Policy = { Min = 4; Max = 5; Letter = 'c' }
         Password = "vscjrl" }
       { Policy = { Min = 1; Max = 3; Letter = 'l' }
         Password = "llrl" }
       { Policy = { Min = 11; Max = 12; Letter = 'w' }
         Password = "wwwwwwwwwwzww" }
       { Policy = { Min = 6; Max = 7; Letter = 't' }
         Password = "wlcktht" }
       { Policy = { Min = 2; Max = 10; Letter = 'r' }
         Password = "rrrrrrrrrwrrrmrr" }
       { Policy = { Min = 2; Max = 6; Letter = 'x' }
         Password = "lhqvpx" }
       { Policy = { Min = 10; Max = 16; Letter = 'h' }
         Password = "kqrhxclktcqhxchg" }
       { Policy = { Min = 6; Max = 10; Letter = 'm' }
         Password = "mmsmkmmjmlmhfmmnmm" }
       { Policy = { Min = 5; Max = 7; Letter = 'h' }
         Password = "hchhhhph" }
       { Policy = { Min = 5; Max = 7; Letter = 'z' }
         Password = "vtzzzwl" }
       { Policy = { Min = 3; Max = 12; Letter = 'z' }
         Password = "zzfzzzzzzzzzz" }
       { Policy = { Min = 7; Max = 9; Letter = 'z' }
         Password = "zzzzzzszzzzzzzzjz" }
       { Policy = { Min = 8; Max = 9; Letter = 'g' }
         Password = "ggggggggg" }
       { Policy = { Min = 13; Max = 16; Letter = 'f' }
         Password = "ptvzfmfkxfdkfhjff" }
       { Policy = { Min = 1; Max = 10; Letter = 'w' }
         Password = "cvhnfgnwpw" }
       { Policy = { Min = 5; Max = 8; Letter = 'd' }
         Password = "fvvmdlfqgjc" }
       { Policy = { Min = 6; Max = 9; Letter = 's' }
         Password = "rzlrwzngshvt" }
       { Policy = { Min = 2; Max = 4; Letter = 'v' }
         Password = "vgql" }
       { Policy = { Min = 1; Max = 3; Letter = 'r' }
         Password = "rrmrr" }
       { Policy = { Min = 5; Max = 7; Letter = 'j' }
         Password = "jkjgwjj" }
       { Policy = { Min = 4; Max = 7; Letter = 'b' }
         Password = "bbbzdzbbcbbb" }
       { Policy = { Min = 4; Max = 10; Letter = 'k' }
         Password = "kkkkrkckkgkkk" }
       { Policy = { Min = 10; Max = 12; Letter = 'm' }
         Password = "mmmmmmddmjmn" }
       { Policy = { Min = 4; Max = 10; Letter = 'k' }
         Password = "mskmvkcpqkk" }
       { Policy = { Min = 5; Max = 10; Letter = 'm' }
         Password = "wbtdmxnvrmwqbqkwmtq" }
       { Policy = { Min = 7; Max = 14; Letter = 'z' }
         Password = "cfzftzzqnxffzh" }
       { Policy = { Min = 12; Max = 13; Letter = 'z' }
         Password = "zzzzkzzzzdzzz" }
       { Policy = { Min = 4; Max = 5; Letter = 'l' }
         Password = "lllslllvl" }
       { Policy = { Min = 5; Max = 8; Letter = 'k' }
         Password = "kkkkkkkkkk" }
       { Policy = { Min = 10; Max = 11; Letter = 'l' }
         Password = "llllllllllwl" }
       { Policy = { Min = 3; Max = 5; Letter = 'v' }
         Password = "hvzpxfvmvcv" }
       { Policy = { Min = 8; Max = 10; Letter = 't' }
         Password = "tbtnrtbqzwtkqtf" }
       { Policy = { Min = 6; Max = 10; Letter = 'j' }
         Password = "njjjpjjjjkjsj" }
       { Policy = { Min = 8; Max = 16; Letter = 'f' }
         Password = "cvpxnsxfdnpdfswdhbb" }
       { Policy = { Min = 6; Max = 12; Letter = 'n' }
         Password = "nnndnnnnnnnz" }
       { Policy = { Min = 2; Max = 3; Letter = 'd' }
         Password = "dzdd" }
       { Policy = { Min = 1; Max = 4; Letter = 's' }
         Password = "jshkscssssssssssssss" }
       { Policy = { Min = 5; Max = 7; Letter = 'k' }
         Password = "kkkqckwkcl" }
       { Policy = { Min = 3; Max = 4; Letter = 'f' }
         Password = "ffdf" }
       { Policy = { Min = 9; Max = 11; Letter = 'c' }
         Password = "cpccccncccqccc" }
       { Policy = { Min = 1; Max = 8; Letter = 'x' }
         Password = "gxxxxgxx" }
       { Policy = { Min = 5; Max = 15; Letter = 'p' }
         Password = "ppvkmmpcvzmmczpz" }
       { Policy = { Min = 12; Max = 13; Letter = 'p' }
         Password = "xppppppppvpnpppp" }
       { Policy = { Min = 7; Max = 12; Letter = 'n' }
         Password = "nwnnnnhcbnjnc" }
       { Policy = { Min = 1; Max = 4; Letter = 'f' }
         Password = "fnzjf" }
       { Policy = { Min = 2; Max = 5; Letter = 's' }
         Password = "tltqss" }
       { Policy = { Min = 3; Max = 10; Letter = 'r' }
         Password = "rrqkrzvkrtbqcrp" }
       { Policy = { Min = 3; Max = 14; Letter = 'h' }
         Password = "hhhhthhhlrwhhhthp" }
       { Policy = { Min = 2; Max = 4; Letter = 'b' }
         Password = "bkbhbq" }
       { Policy = { Min = 15; Max = 16; Letter = 'v' }
         Password = "vvvvvvvvvvvvvvvsvvv" }
       { Policy = { Min = 1; Max = 17; Letter = 'h' }
         Password = "vtjjhtxrchshpxhsh" }
       { Policy = { Min = 4; Max = 7; Letter = 'n' }
         Password = "jnpnpnn" }
       { Policy = { Min = 3; Max = 4; Letter = 'h' }
         Password = "jvhz" }
       { Policy = { Min = 4; Max = 5; Letter = 'w' }
         Password = "wcpzw" }
       { Policy = { Min = 9; Max = 10; Letter = 'q' }
         Password = "tvxbsfmqqblhq" }
       { Policy = { Min = 3; Max = 5; Letter = 's' }
         Password = "jssstxfbsssshssgkss" }
       { Policy = { Min = 3; Max = 9; Letter = 'r' }
         Password = "fnrhqkrmtstqjgc" }
       { Policy = { Min = 12; Max = 15; Letter = 'n' }
         Password = "xqwnnnnnnnnmnnn" }
       { Policy = { Min = 13; Max = 15; Letter = 'q' }
         Password = "qqqqqqqqqqqqqqtq" }
       { Policy = { Min = 3; Max = 4; Letter = 'd' }
         Password = "dcdl" }
       { Policy = { Min = 4; Max = 12; Letter = 'd' }
         Password = "vrldnmpndmlgdzrv" }
       { Policy = { Min = 2; Max = 4; Letter = 'h' }
         Password = "mhhh" }
       { Policy = { Min = 3; Max = 4; Letter = 'f' }
         Password = "fcvfc" }
       { Policy = { Min = 1; Max = 2; Letter = 'w' }
         Password = "whwwwz" }
       { Policy = { Min = 7; Max = 8; Letter = 'm' }
         Password = "mpmlmmmmhdbh" }
       { Policy = { Min = 2; Max = 4; Letter = 'q' }
         Password = "qxbqqdsjrdpxf" }
       { Policy = { Min = 6; Max = 14; Letter = 'r' }
         Password = "wbmlhrcgrgrkzqfj" }
       { Policy = { Min = 2; Max = 7; Letter = 'c' }
         Password = "ghcvcdcmcztckct" }
       { Policy = { Min = 2; Max = 9; Letter = 'n' }
         Password = "nnnnnnnnpnnn" }
       { Policy = { Min = 3; Max = 5; Letter = 'f' }
         Password = "zlgffv" }
       { Policy = { Min = 1; Max = 6; Letter = 'm' }
         Password = "ntmmmm" }
       { Policy = { Min = 2; Max = 4; Letter = 'w' }
         Password = "jgqwv" }
       { Policy = { Min = 5; Max = 12; Letter = 'f' }
         Password = "gscfzhmrtxfw" }
       { Policy = { Min = 5; Max = 7; Letter = 'r' }
         Password = "rwzklcrnrrg" }
       { Policy = { Min = 8; Max = 10; Letter = 'h' }
         Password = "hhhzhhhpxhhh" }
       { Policy = { Min = 9; Max = 11; Letter = 'x' }
         Password = "xxxxxxxxfxbx" }
       { Policy = { Min = 7; Max = 8; Letter = 'q' }
         Password = "qdnqnzbq" }
       { Policy = { Min = 2; Max = 10; Letter = 's' }
         Password = "sssmssslbb" }
       { Policy = { Min = 8; Max = 9; Letter = 'n' }
         Password = "wgfnghnlnkf" }
       { Policy = { Min = 4; Max = 10; Letter = 'd' }
         Password = "dddsdddlds" }
       { Policy = { Min = 1; Max = 5; Letter = 'k' }
         Password = "bfkkkn" }
       { Policy = { Min = 2; Max = 5; Letter = 'w' }
         Password = "wwwwww" }
       { Policy = { Min = 14; Max = 16; Letter = 's' }
         Password = "bjszbzmcnsvplsrh" }
       { Policy = { Min = 8; Max = 9; Letter = 'b' }
         Password = "bbjbbbbbbvvbbx" }
       { Policy = { Min = 2; Max = 10; Letter = 'm' }
         Password = "dmnrsmtqkf" }
       { Policy = { Min = 7; Max = 12; Letter = 'f' }
         Password = "fbtwftvffsgfwlnw" }
       { Policy = { Min = 9; Max = 10; Letter = 'h' }
         Password = "shhhpshfxhbrdhshh" }
       { Policy = { Min = 4; Max = 9; Letter = 't' }
         Password = "tgpdtwrmt" }
       { Policy = { Min = 2; Max = 6; Letter = 't' }
         Password = "vhtwntl" }
       { Policy = { Min = 3; Max = 5; Letter = 'j' }
         Password = "ljjjd" }
       { Policy = { Min = 2; Max = 3; Letter = 'w' }
         Password = "hxwvbxwwbwsvc" }
       { Policy = { Min = 7; Max = 8; Letter = 'r' }
         Password = "rrrzrrnr" }
       { Policy = { Min = 3; Max = 4; Letter = 'x' }
         Password = "jxjh" }
       { Policy = { Min = 7; Max = 12; Letter = 'w' }
         Password = "mjmbtgntdwjwnqztv" }
       { Policy = { Min = 5; Max = 6; Letter = 'l' }
         Password = "vlvllt" }
       { Policy = { Min = 7; Max = 8; Letter = 'n' }
         Password = "nnnnnnpnnnn" }
       { Policy = { Min = 3; Max = 10; Letter = 'c' }
         Password = "wcgcxzcdwmcn" }
       { Policy = { Min = 16; Max = 18; Letter = 'h' }
         Password = "hchhhhhhhhhhhhhcmh" }
       { Policy = { Min = 5; Max = 11; Letter = 'f' }
         Password = "fflffffffflfff" }
       { Policy = { Min = 3; Max = 13; Letter = 'z' }
         Password = "zzzzzzzzzzzzpzz" }
       { Policy = { Min = 6; Max = 9; Letter = 'k' }
         Password = "kkkkkskkkk" }
       { Policy = { Min = 6; Max = 15; Letter = 'c' }
         Password = "ccccccccbccccctccc" }
       { Policy = { Min = 9; Max = 18; Letter = 'p' }
         Password = "klcpzpdwzvpqppspfpp" }
       { Policy = { Min = 10; Max = 13; Letter = 'b' }
         Password = "pbbbbmbbdbwtmd" }
       { Policy = { Min = 10; Max = 11; Letter = 'v' }
         Password = "xvvvvvvvvnnvv" }
       { Policy = { Min = 2; Max = 4; Letter = 'm' }
         Password = "msmmm" }
       { Policy = { Min = 1; Max = 4; Letter = 'w' }
         Password = "rwwlwrwrwwrfngc" }
       { Policy = { Min = 8; Max = 9; Letter = 'r' }
         Password = "rjjlddjrnbr" }
       { Policy = { Min = 13; Max = 16; Letter = 'd' }
         Password = "pzdfzqbwclbjddxtvddf" }
       { Policy = { Min = 14; Max = 15; Letter = 'q' }
         Password = "qqqqqqqqqqqqqbq" }
       { Policy = { Min = 12; Max = 14; Letter = 'k' }
         Password = "kkkjgrkkqkkkkl" }
       { Policy = { Min = 3; Max = 4; Letter = 'd' }
         Password = "gsdnkdfnf" }
       { Policy = { Min = 8; Max = 17; Letter = 'h' }
         Password = "glhfvrshlrqwdrfrh" }
       { Policy = { Min = 2; Max = 12; Letter = 'l' }
         Password = "mflqfvxfgzkmd" }
       { Policy = { Min = 5; Max = 8; Letter = 'f' }
         Password = "ckllfnfbflqgrsd" }
       { Policy = { Min = 1; Max = 17; Letter = 'm' }
         Password = "kckvffhnlmjvdtgpm" }
       { Policy = { Min = 16; Max = 17; Letter = 'p' }
         Password = "pplppppcppppppppppp" }
       { Policy = { Min = 5; Max = 8; Letter = 'h' }
         Password = "hhjbmplh" }
       { Policy = { Min = 7; Max = 10; Letter = 's' }
         Password = "jsjlwgsssbsvfsvk" }
       { Policy = { Min = 2; Max = 8; Letter = 'x' }
         Password = "xpfxbqxxqxhdrxhqm" }
       { Policy = { Min = 12; Max = 16; Letter = 'n' }
         Password = "nnnnnznnnnnnnnnmnvn" }
       { Policy = { Min = 6; Max = 12; Letter = 'v' }
         Password = "vvvvvvvvvvvgv" }
       { Policy = { Min = 8; Max = 9; Letter = 'j' }
         Password = "pjjjjmjnj" }
       { Policy = { Min = 16; Max = 17; Letter = 'h' }
         Password = "hhhbhjhrhhhhxhhgt" }
       { Policy = { Min = 3; Max = 11; Letter = 'd' }
         Password = "ldpmvddhdrdjdj" }
       { Policy = { Min = 6; Max = 7; Letter = 'n' }
         Password = "nnnnnnnn" }
       { Policy = { Min = 5; Max = 8; Letter = 'f' }
         Password = "tglffvhgnfxzfhf" }
       { Policy = { Min = 13; Max = 18; Letter = 'r' }
         Password = "rrrrrrrrrrbrhrrrrrrr" }
       { Policy = { Min = 19; Max = 20; Letter = 'n' }
         Password = "nnnnnnnnnnnnnnnnnnnj" }
       { Policy = { Min = 7; Max = 8; Letter = 'w' }
         Password = "tpmmxqsw" }
       { Policy = { Min = 5; Max = 7; Letter = 'c' }
         Password = "ccccccr" }
       { Policy = { Min = 9; Max = 10; Letter = 'l' }
         Password = "qltnnlnfllqlw" }
       { Policy = { Min = 6; Max = 7; Letter = 'g' }
         Password = "xggbggz" }
       { Policy = { Min = 7; Max = 10; Letter = 's' }
         Password = "sssssfcssss" }
       { Policy = { Min = 5; Max = 7; Letter = 'j' }
         Password = "jsjkxwqhjcvjtwjzl" }
       { Policy = { Min = 10; Max = 14; Letter = 't' }
         Password = "qdtttzttcvtttnn" }
       { Policy = { Min = 12; Max = 13; Letter = 'b' }
         Password = "bbbbbbrbbbbqb" }
       { Policy = { Min = 1; Max = 15; Letter = 'd' }
         Password = "dshhrjkwcjjhlthdts" }
       { Policy = { Min = 7; Max = 12; Letter = 'p' }
         Password = "hrxkphmqpvpptpqbw" }
       { Policy = { Min = 13; Max = 14; Letter = 'd' }
         Password = "ddndddxdtdrkvldd" }
       { Policy = { Min = 3; Max = 4; Letter = 'h' }
         Password = "htht" }
       { Policy = { Min = 7; Max = 8; Letter = 'c' }
         Password = "xtsvzccfckccx" }
       { Policy = { Min = 4; Max = 5; Letter = 'r' }
         Password = "gstrwshptzrdtjj" }
       { Policy = { Min = 7; Max = 8; Letter = 'b' }
         Password = "wbbnbbbm" }
       { Policy = { Min = 15; Max = 17; Letter = 'c' }
         Password = "cgpqxbccqcjpzlcctmx" }
       { Policy = { Min = 2; Max = 7; Letter = 'k' }
         Password = "kvtqqmsx" }
       { Policy = { Min = 8; Max = 11; Letter = 's' }
         Password = "ssxssssqsssssssss" }
       { Policy = { Min = 3; Max = 9; Letter = 'd' }
         Password = "ddddddddld" }
       { Policy = { Min = 13; Max = 16; Letter = 'p' }
         Password = "pppppppppppppppwpj" }
       { Policy = { Min = 6; Max = 8; Letter = 'v' }
         Password = "sxkghpckvb" }
       { Policy = { Min = 17; Max = 18; Letter = 's' }
         Password = "ssssssssbsssssssksss" }
       { Policy = { Min = 1; Max = 2; Letter = 'w' }
         Password = "wlwxdsw" }
       { Policy = { Min = 8; Max = 9; Letter = 'q' }
         Password = "qqqqqqqqnq" }
       { Policy = { Min = 9; Max = 16; Letter = 'f' }
         Password = "fjdsfvkfqffffjcfpff" }
       { Policy = { Min = 12; Max = 13; Letter = 'h' }
         Password = "bhhhhhhfwhphhhhhh" }
       { Policy = { Min = 7; Max = 8; Letter = 'k' }
         Password = "kkkkkknkkkkkkk" }
       { Policy = { Min = 4; Max = 7; Letter = 'w' }
         Password = "wwwfpsw" }
       { Policy = { Min = 8; Max = 11; Letter = 'd' }
         Password = "rsndldddddxddmf" }
       { Policy = { Min = 2; Max = 10; Letter = 'c' }
         Password = "cjcdcccccc" }
       { Policy = { Min = 6; Max = 7; Letter = 'v' }
         Password = "zvnrhth" }
       { Policy = { Min = 3; Max = 8; Letter = 'z' }
         Password = "zzxzzzzzdzjzzzzz" }
       { Policy = { Min = 11; Max = 12; Letter = 't' }
         Password = "tctdttttwtrtttttjth" }
       { Policy = { Min = 8; Max = 9; Letter = 'c' }
         Password = "ccccccccrccc" }
       { Policy = { Min = 17; Max = 18; Letter = 'p' }
         Password = "pppppppppppppppppvp" }
       { Policy = { Min = 3; Max = 8; Letter = 'l' }
         Password = "svlmlkspljr" }
       { Policy = { Min = 1; Max = 2; Letter = 'n' }
         Password = "nwnkq" }
       { Policy = { Min = 1; Max = 11; Letter = 'j' }
         Password = "jjjjjjjjjjjj" }
       { Policy = { Min = 18; Max = 19; Letter = 'g' }
         Password = "ggggggggggggggggggr" }
       { Policy = { Min = 10; Max = 11; Letter = 'j' }
         Password = "jjjjjcdjgjv" }
       { Policy = { Min = 3; Max = 7; Letter = 'p' }
         Password = "ptttppppppj" }
       { Policy = { Min = 2; Max = 5; Letter = 'd' }
         Password = "cdndsd" }
       { Policy = { Min = 6; Max = 10; Letter = 's' }
         Password = "sssssmssssss" }
       { Policy = { Min = 15; Max = 16; Letter = 'k' }
         Password = "vxwxxhhkkhklqksd" }
       { Policy = { Min = 3; Max = 4; Letter = 'x' }
         Password = "rpxn" }
       { Policy = { Min = 1; Max = 6; Letter = 'g' }
         Password = "vmgckg" }
       { Policy = { Min = 3; Max = 4; Letter = 'j' }
         Password = "jjbs" }
       { Policy = { Min = 5; Max = 10; Letter = 'd' }
         Password = "qrnmbddndvcmdsjjbdhd" }
       { Policy = { Min = 7; Max = 9; Letter = 'v' }
         Password = "vvmgvvvpvm" }
       { Policy = { Min = 1; Max = 7; Letter = 'z' }
         Password = "zzzzzzwzzzz" }
       { Policy = { Min = 4; Max = 7; Letter = 'n' }
         Password = "nnnnnnqn" }
       { Policy = { Min = 8; Max = 9; Letter = 'k' }
         Password = "kwkknknkrkgkbklmpb" }
       { Policy = { Min = 1; Max = 5; Letter = 'z' }
         Password = "zzmzfzz" }
       { Policy = { Min = 6; Max = 10; Letter = 'm' }
         Password = "mmmmmfmmmm" }
       { Policy = { Min = 9; Max = 11; Letter = 's' }
         Password = "sssssstsssgss" }
       { Policy = { Min = 2; Max = 6; Letter = 'n' }
         Password = "nnfnpgnnnmnnn" }
       { Policy = { Min = 15; Max = 17; Letter = 'w' }
         Password = "wwwwrswthgwhkwwrw" }
       { Policy = { Min = 5; Max = 9; Letter = 'h' }
         Password = "lbhdhplmbnwh" }
       { Policy = { Min = 5; Max = 6; Letter = 'd' }
         Password = "jdddqqt" } |]
    |> Array.toList

let isOk1 (row: Row): bool =
    let realCount =
        row.Password
        |> String.filter (fun c -> c = row.Policy.Letter)
        |> String.length

    (realCount >= row.Policy.Min)
    && (realCount <= row.Policy.Max)

let part1 (input: Row list): int =
    input |> List.filter isOk1 |> List.length

let isOk2 (row: Row): bool =
    let firstChar = row.Password.[row.Policy.Min - 1]
    let secondChar = row.Password.[row.Policy.Max - 1]
    let firstOk = firstChar = row.Policy.Letter
    let secondOk = secondChar = row.Policy.Letter
    (firstOk || secondOk) && not (firstOk && secondOk)

let part2 (input: Row list): int =
    input |> List.filter isOk2 |> List.length

let main = part2 realInput
