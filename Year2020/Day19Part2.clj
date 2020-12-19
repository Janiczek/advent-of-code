;; Needs deps.edn with this inside:

;; {:deps
;;  {instaparse/instaparse {:mvn/version "1.4.10"}}}

(require '[instaparse.core :as insta])

(def source 
  "
S = x8 x11
x66 = x26 x127 | x132 x49
x65 = x99 x132 | x100 x26
x40 = x1 x26 | x16 x132
x87 = x16 x26 | x49 x132
x124 = x132 x57 | x26 x107
x80 = x1 x26 | x53 x132
x52 = x26 x24
x58 = x43 x132 | x26 x26
x72 = x49 x26 | x16 x132
x82 = x26 x132
x63 = x26 x135 | x132 x127
x113 = x25 x26 | x134 x132
x50 = x26 x21 | x132 x135
x35 = x26 x12 | x132 x106
x78 = x132 x123 | x26 x14
x39 = x49 x26 | x82 x132
x104 = x132 x26 | x26 x26
x88 = x3 x132 | x127 x26
x90 = x82 x26 | x16 x132
x96 = x104 x26 | x16 x132
x98 = x24 x26 | x104 x132
x53 = x43 x43
x27 = x5 x132 | x127 x26
x91 = x93 x26 | x135 x132
x57 = x132 x118 | x26 x125
x25 = x49 x26 | x21 x132
x22 = x132 x1 | x26 x49
x135 = x132 x132 | x26 x132
x120 = x80 x132 | x81 x26
x132 = 'a'
x10 = x26 x60 | x132 x124
x97 = x132 x7 | x26 x10
x130 = x103 x132 | x66 x26
x41 = x58 x26 | x16 x132
x47 = x81 x26 | x18 x132
x111 = x132 x127 | x26 x3
x119 = x132 x62 | x26 x36
x126 = x26 x16 | x132 x49
x76 = x4 x26 | x30 x132
x17 = x49 x26 | x24 x132
x67 = x26 x58 | x132 x53
x131 = x127 x26 | x135 x132
x71 = x26 x112 | x132 x115
x15 = x98 x132 | x6 x26
x114 = x74 x26 | x101 x132
x60 = x70 x132 | x19 x26
x45 = x132 x27 | x26 x40
x105 = x135 x26 | x16 x132
x24 = x43 x26 | x132 x132
x18 = x82 x26 | x135 x132
x85 = x135 x132
x56 = x132 x69 | x26 x113
x89 = x13 x26 | x122 x132
x37 = x82 x132 | x58 x26
x106 = x26 x49 | x132 x21
x125 = x26 x5 | x132 x24
x55 = x37 x26 | x2 x132
x8: x42 | x42 x8
x81 = x26 x16 | x132 x21
x107 = x26 x73 | x132 x131
x112 = x26 x67 | x132 x90
x84 = x26 x48 | x132 x71
x49 = x132 x26
x44 = x24 x132 | x5 x26
x1 = x132 x26 | x26 x132
x28 = x79 x26 | x87 x132
x68 = x9 x26 | x89 x132
x7 = x26 x76 | x132 x68
x115 = x26 x128 | x132 x72
x79 = x49 x132 | x104 x26
x6 = x26 x21 | x132 x16
x136 = x26 x129 | x132 x121
x33 = x132 x116 | x26 x110
x116 = x39 x26 | x105 x132
x117 = x56 x132 | x136 x26
x16 = x26 x132 | x43 x26
x133 = x132 x5 | x26 x1
x86 = x26 x104 | x132 x135
x51 = x86 x132 | x137 x26
x127 = x132 x132 | x132 x26
x34 = x132 x130 | x26 x83
x94 = x132 x127 | x26 x49
x137 = x132 x135 | x26 x53
x100 = x26 x34 | x132 x114
x19 = x26 x85 | x132 x64
x12 = x132 x21
x73 = x93 x132 | x93 x26
x2 = x26 x1 | x132 x24
x74 = x132 x126 | x26 x95
x62 = x132 x55 | x26 x20
x38 = x26 x84 | x132 x119
x14 = x108 x26 | x94 x132
x75 = x82 x132
x61 = x16 x132 | x3 x26
x99 = x132 x33 | x26 x109
x59 = x1 x26 | x93 x132
x77 = x132 x117 | x26 x102
x21 = x132 x132
x122 = x26 x127 | x132 x93
x31 = x38 x26 | x77 x132
x95 = x135 x132 | x1 x26
x36 = x26 x120 | x132 x23
x30 = x61 x132 | x41 x26
x54 = x135 x26 | x53 x132
x70 = x96 x132 | x54 x26
x26 = 'b'
x29 = x3 x26 | x104 x132
x23 = x132 x52 | x26 x88
x9 = x132 x37 | x26 x32
x32 = x24 x132 | x53 x26
x103 = x132 x127 | x26 x5
x64 = x104 x43
x5 = x132 x132 | x26 x26
x102 = x78 x132 | x46 x26
x123 = x73 x132 | x80 x26
x42 = x97 x132 | x65 x26
x3 = x132 x43 | x26 x132
x4 = x132 x92 | x26 x29
x134 = x132 x21 | x26 x93
x93 = x26 x26
x20 = x132 x59 | x26 x105
x13 = x26 x58 | x132 x5
x43 = x132 | x26
x110 = x125 x26 | x12 x132
x109 = x132 x35 | x26 x47
x69 = x22 x26 | x44 x132
x83 = x111 x26 | x63 x132
x11: x42 x31 | x42 x11 x31
x121 = x132 x17 | x26 x75
x128 = x58 x132 | x53 x26
x92 = x53 x26 | x93 x132
x108 = x26 x93 | x132 x93
x46 = x132 x45 | x26 x51
x118 = x82 x26 | x58 x132
x48 = x15 x132 | x28 x26
x129 = x132 x50 | x26 x91
x101 = x132 x131 | x26 x133
  ")

(def messages 
    [ 
      "abbbbaaabbbbaabbabbbaabaababbaababbbabbb"
      "bbaaaabbaaabbbbaaaaaabab"
      "baabaaabbaabbbaaaababaaaaaabbbababaaabaabbbbaabaaabbaabbbbaaabba"
      "aaabaaabaabbabbabbabaaba"
      "bbaaabbbbbbabaabaaabaabb"
      "aaabaabaabaaaababaababba"
      "aababbbbababbabbbabaaaaaaabbaabaaabbbaaa"
      "baababababaabaaababbbaaa"
      "aaabbaaaaababaabbbbabbba"
      "baaabaaaabbbaabbbbbbbbbb"
      "baaabaaabbaabbbabbbbababaabbaabb"
      "aaabbbababbbbbaaaabbbabaaaabababbbaaabba"
      "aaabaaaabbbabbbabbabaabb"
      "aabaaabaaaabaabaabbbbabb"
      "bbaabbbabababbbaabaaabaabbbbaaab"
      "abbaaaaaaaaaabaabaababababbaaaba"
      "babaaaaabaaababbabaabaaaabbababbaaaabbba"
      "aabbbbbbbababbbbbabaabbbaabbabab"
      "bbbabbbabbababbbabbbbbbabaaaabaaaaaabbba"
      "aabbbbbaababbabbbbbabaabaabbabbaabbabbabbababbaa"
      "aabaaaaabaaabbaaabaaabababbbabaaabbaaabababaabaa"
      "ababbaaaabbaabaaaababaabbaaabbab"
      "abbbbaaaabbaaaaababbbabbbaabaabaaababbbababbbbbbaabbbaabbbbbbbabbabbaaab"
      "aabaaabaabbabababbbbabbbbaabbbab"
      "babaabbaabbaabaababababa"
      "abbbaaaaaaabaaababbababbbabbabba"
      "bbabbbbaabababaababababb"
      "babaaaaaabbbaaaabbbababa"
      "aabbaaabbbaaabbbababaabb"
      "aabbbbaabbbabbaababbbaab"
      "ababbbbbabaabbaaaaaaabaabbabaabbaabbaaaa"
      "aababaaabbaaababbbabbaaaabbabbbbbabbabaababaabab"
      "aabbbabaaababbbabbababaaaaabbabbababbbbbbaababbb"
      "bbabbaabbbaabaabbbaaabbbabbbabbabaaabaaababababbabbbaabbbbbabaab"
      "abababaaaababaaaabaababb"
      "bbbbababbbaabbbaabaaababbbabbbaa"
      "aaabbaaababaaaababbabbab"
      "bbaabbbabaaaaaababaabbabbbbaaabbbbbbbbabbaaaabbbaaaaaaba"
      "abaaabbbbabbbabbaabababaaaaaaaaa"
      "ababbabbabbababbbbbaabbbbbbbabbaababaabbbabbbaaaaaabaababbaaaabbbaabbabbbbbaababbbbaaaaa"
      "aabbabaaabbbaaaabbbababb"
      "bbaaababbbaaabbbabbabaab"
      "aaaaabaaabbbbabaababbbbaabaabaaaaaabbbbb"
      "abbbaabbaababbbbabaababb"
      "baaababbbabaaabbbaabbaab"
      "bababbbbbaababbaabaabbbaaaaabbbbabaabaabbabbbbab"
      "abaabbbaabaaaabbababaabb"
      "bbbaabbbababbbbbaababaaabababbbbbbbbbbabbabaabaa"
      "aaabbbaaabbbaaaaabaabbaabbabbaab"
      "bbabbaababababbaabaaabbb"
      "bbbbbaaaabbabbbbaaabaaaa"
      "abbbbbaababbbbaabbbababa"
      "abbabbaabbabbbbababababa"
      "aabbabbaabaabaaabbbaabaaaaaabbba"
      "abbabbbaaaabbbaabaaaaaaaaabbaabb"
      "aabbbbaaabaaaabbaabbaaba"
      "aababaababaabaaabaaabbaabbbbaaabbbbbbabb"
      "bababaaaabbbbbaaaabaabbb"
      "babaaaabbabaaabbabaabbaaaaaaabbabababaaabaabbabb"
      "aabbbbaaaaaaabbaaabbaaaa"
      "abbbaabbbaaaaaaabaabbbaa"
      "abababaabbabbaaabbbaabaaababaaaaaabaabbbababaabbabbaaaab"
      "abbbabaaaababbbaabbbbbaaababaaba"
      "bbbbbaaaaaabaaababababab"
      "bbaaaabbaabaabbaaaabaaabbaabbbaa"
      "abbbabbaaaaaaabbabbbaabbaaabaabb"
      "aaaabaabaaaaabbabaabbaba"
      "abbbbabaaaaaaabbbbbbaaaaaaaabaababaababa"
      "bbaaabbbaabaababbbabbaab"
      "ababbbbbabbbbbabbbaaaabbbbbbbaab"
      "aaaaabaaaabaaaabbaaabaaaaababbab"
      "abbabbaaabbbaaaabbabbaab"
      "ababbabbaababbbbaababaaaabbaaaaa"
      "abaaababaaabbabbbaaaaabbabaabbaaabbbabbaaaaababa"
      "aabbbabababbbbbbaabbaaaa"
      "ababbbababaaaabbbbbaabbbabbbabaabbbaabbbababaaab"
      "aabbabbbbbabaaaabbaaaabbbababbaa"
      "abaabbaabbbaaabbababaaaaabaabaaaabaabbababaababb"
      "baaabaaaaaabbabbabbbabbb"
      "abbbabbaabaabbaabbbaaabaaaabaababbaabaab"
      "aababbbbabbaabbbaaaabbba"
      "bbbbabbabbabbaabbbbaaaababaaabaabbaaaababbbaababaaababab"
      "bbbbbaaaabaaaabbbaaaabbb"
      "abbabababbbabbbbabbabaab"
      "babaaaabbbaabbbbabaabbbb"
      "bbbaabbbabbbbbaabbbaabab"
      "abaabaaaabbbabbbbabaaaaaaaabbbbaabbaababaaaaababbaaaaaab"
      "baaabaaabbbaaabaaaaabbbbaabbbbbaaaabbbaababbbbaaabababababbbbaab"
      "aabaaaabbaaaaaabaabaaabbaabbbababbaaaaaa"
      "baaabbabaabbbbbbbaaababbbabbabbb"
      "baaabbababbabbaabbbaabbbbaabaabaaaaaabaabbababbbbabbabaabaaababb"
      "aabbabbbbbbbbbabbaabbaab"
      "aabbabbaaabbabaabababaab"
      "babaaaaabbabaaaabbaaababaaabbaaabbaaaaabbaaabaab"
      "abbaabababbabababbabbabababbaabb"
      "abbbabaabbbaaabbabaabbbaaabababb"
      "bbbbaaaaaabbbbaaaaabaaabababaaaaaabaabbbababbaab"
      "aabbabbabaabbababbabbaab"
      "abaaaaabaaaaaabbabaaababbbababbabbbbbbaa"
      "abababbabbaaaaabbabbaabb"
      "baababbabbaababaaaababbb"
      "ababbabbaababaababaababb"
      "baaaaabbabaabbaabbaabbbbaaaabbaa"
      "abaaaabbaaabaababbabbaaaaaaaabaaabaaaabaaabbbbabaaaababa"
      "aabbaaabaabaabaababbabab"
      "ababaaaaaabbbabababaaabaaabababb"
      "aabaaabbababbbababaaabaa"
      "bbbbbbbababaaabbaaaabaaabbabbaabbbbbbaabaabbbbababbbaababaababaa"
      "baaababababbbbaaaaababbabbaaaaaaaabbbabb"
      "bbbbaabbababbbabbabaaaabbbbbbbbb"
      "aaabaaababababaabaaaaaabbaaabbabbbbbbbbb"
      "babbbbbabaaaaaababbbabbaababbbbbaaabbaba"
      "aababaabbaaabbaabbbbabbbabaaaabaaabbabab"
      "abbabababaaabbbbabbbbababbbababb"
      "abbabbbbabbaabaabababbba"
      "bbaaabaabababaaaaaaaabbb"
      "aaabbbaabaabaababbbbbbbb"
      "aabbaaabaababaaaabbaabaaaabababb"
      "abbbaabbabaaababaabbaaaa"
      "aaabbbbaaabaaabaabaabaaaaaaaaabbbbababaabbabbbaaabbaaababaabbbab"
      "bbbaaaaaabaabbababbabbaaaaababbaabbabbbbabaaaaaabbaaaaba"
      "aabbbbbaabbaababbbababbb"
      "bababbbbababbababaaabbaaabbabbbbaaababbbbbbbbbaabaaaabbb"
      "ababbbbaabbaabbbabbaabbabbabbaababaababb"
      "abbbaaaababaabbbbabaaabaaaaaabab"
      "aabbabbaabababbabaabababbbaaabba"
      "aabbaabababbabbbbabbbaba"
      "abababaaaabaaaaaababbbbababababa"
      "abaabaaaaaabbbabaababaabababbaba"
      "aabbabbaaababababaabbaaabaababbaaaaaabab"
      "abaaabbaaababaaaaabbbabb"
      "babbbbaaabaaaaababaabbbabbbbaabaaababbaaabbabbab"
      "abbaabbabababbbbbabbbaab"
      "baabbaaaaaaababbabbabbaabbababba"
      "babaabbbbbabbaaabbaaaabbbbababbb"
      "aaabbaaabbbaaabaaabbabbaaaabbabbbbbaaaababbbbbba"
      "bbbaaaaaaaabbbaababaabbabaaaaaaaabaababbbaaaabbb"
      "baaaaaaaabbababbbabbabaaabbbbaabaabaaabbabbabaabaaabbaabbbbabbbaabaababbabbaaabaaabaaaab"
      "bbababaaababaaaaababbbaa"
      "bbbbaaaabbabbabaabbaaaab"
      "bbabbbaaabbabababbabbbbaaabbabbababbbbbbaabaaaba"
      "babbbabaabbbbbbbbbbabbaabababaaa"
      "bababbbbaaaaaabbbabaabbbaabbabbbbabbabab"
      "abbabbaabbbbaabbaaaaaaba"
      "abababaaaababbbbaababbab"
      "abbbbbababaaaabbbbbbbbba"
      "aabbabaaaabbaaabaaababbabbbaabbbbbabbaabbbbbbabb"
      "abaaaaabbbabababbaabbbaabbbbaaaabbbbbabb"
      "bbbaaaabbbaaabababaaabababaaabaa"
      "bbaaabbbbaababbbbbabaaabaaaababaaaaabbab"
      "bbbabbaaaababaabbbabbbab"
      "abaaaababbbbbbaaababaaabbbbaabbbabbbbaabbabaabaaababaaaaabbabbaa"
      "bbbaabaaabaaabbbbbaaaaaa"
      "babaabbbabbbabbabbaaaabbabababbabaabbbaababbaaab"
      "ababbbbaabbabbbbbabaabab"
      "aaaabaabaababaaaabbbaaba"
      "bbbabbaaaabaaaaabaababaa"
      "abababbaaaabaababbabbabaabaaaaababbbbaaa"
      "babaabbababbbbaaabbabbbb"
      "bbbbaabbaaaaaabbbbbababa"
      "aabaaabbaaaaaabbababaaab"
      "bababbbbabbaabbaaaabbbbaaababaaabbbaaaabbbabbbbb"
      "bbbabbaabbaabababababaaabaaabaabbbabbbab"
      "bbaabababaababbabbabbaaaabaaabababbaaaab"
      "ababaaaabbababaababaaabaabbaabbabbaaaaba"
      "babbbbbababbbbbabbbabbababaabababbbaabba"
      "abaabbaaabaaabbabbaaababbbbbaaaaabaababaaaaabbaa"
      "bbbabbaabbaabbaabbaabbbbbbbbabaabaababbb"
      "baaaaabbbbbabbbbbaaabbba"
      "bbbabaababbbbbabbbababab"
      "abbbaaaaabbabbaaaabaaabaabbbbaaa"
      "abbbaabbbbbaaaabbbabbaaabbbabbbbbbababbbaaaababaabbbbaab"
      "bbbaabbbabaaababbbbbaaab"
      "abbbbbaaaabababababaabaa"
      "abbaabbabaababaaababbababbaaabaaabbababbbbbbbbaababaaaaaaaabbbbbbbabaaba"
      "bbbaaaaaabbababbbbababbb"
      "aaabbbbaabaaabbaabbabaaaaaaaaaaa"
      "aaaabbbbabbaabaabbabaaaaabaaaabbbbbbbbab"
      "abbbabaaaabbabbaaaaabbaa"
      "babbbabbaabbbbaabaaaaabbabaaabaa"
      "bbaabbaabbbaaaaabbbababa"
      "bbaabbaaabaabaaaaaaaaabbbaabbaba"
      "bababbbbbbabbaaaaaaabaaa"
      "bbabbaaaabbaaaaabbabaabb"
      "bbbaaabbbabbabaaabababbbbbbababbbbbabaaa"
      "babbbbbaababbbababbabbbabaaaabbbabaaabaa"
      "bbaababbbbabbbbababaaabababababbaaababab"
      "ababbabaabababaaaabaabaaaaaababb"
      "babaabbabaaaaaabbaabbaba"
      "aabaaabbaaaaabbababbbabbaabaaabbbababbababbabaabbbabbbab"
      "abbababaababaaaabbaabaab"
      "ababbbbabbbaabbbbbbbbabb"
      "bbaababbbabaaaaabaabbaaabaaaabab"
      "abaabbbaaabaaaaabaabaaaaababbaaabbbabaaaaaabaabb"
      "aabaaaaabbbbbbabaabbabbbaabaaaaaabbbbaab"
      "bbaaaaabbbaaababbbbbbbabaababbbabaaaaaba"
      "aabbbaabaaabbaaaabbbabaaabaabbbabababaab"
      "ababbbaaabbbbbaaabaabbbabbbabaaababababbabbabaabbabbbaabbbbaaabb"
      "bbaaaaabaabaaaaaababaaab"
      "aaabaaabaabaaaabaaabaabb"
      "abbaaaaabbbabbaaaaaaabab"
      "aabaaaabbbaaaabbbbbaaaaaabbaabaaabbabaabaaaabaaa"
      "aaabaababbaabbaaababbbbbbaaaabab"
      "bbaaaaabbababaaababbaaab"
      "abaabbbaaaaabbaaababaaabbaabaabb"
      "aababaabbbababaaabbaababbbbaabbaabbbbabb"
      "abbaabbbaabaaabaaabababb"
      "abbaababbabaabbbbbbaaabbbbbbaaab"
      "aababababababaaabbbbbbba"
      "ababbbbaabbaabbbbaaaabbb"
      "baabaaabbbaaabbaaaaabbbabbbaaaabaabbabbb"
      "abaabbbababaabbbbbbbabba"
      "abbbbbababaabaaabbbaabaabbabbabb"
      "bbbbbbabababbbababaabbabbaabbbbbaaabaabb"
      "abbaabababbababbaabbbabaaaabbbbaabbaabbbbbbababbbbaaaaaa"
      "babaaaaaaababbbabaabbaaabbabaaabababaaab"
      "aaaaabbaaabbbbaabaaaaaaabbbaaabaabbaaaba"
      "aabaababbabaaababaabbabb"
      "aababbbbababbabbbbbaaaaabbbababaaabababb"
      "bbabbabaabbbbabaabbbbbba"
      "babaaabbaaaaaabaaababbabaaabbbbb"
      "bbbbabaaaabaaabbaaaababbbbbbabaabbbaaaabababaabb"
      "abaabbabbbbbaaaaaababbbbaababbaa"
      "abbabababbbaaabaaaabbbbb"
      "baaaaaababbaaaaabaabababbabbbbbaaabababb"
      "bbbaaaababbbaaaababbbbab"
      "bababbbbbaabaababbbabaababbbabababbbabab"
      "aababababbbbaabababaaaaa"
      "baaababbbababbbbaabaaabbaaaaaaab"
      "abbabbbaabbbaaaaabbaabababaaabaa"
      "abababaabbaabbbbbbbbbbabaaaabaaa"
      "bbbbabbbaababbbbabbababbabaaaabaaabaabbb"
      "bbaababbaabaaabababbaaabbaabaabbaaaaaaabbaabbbaabbbbbbbb"
      "babbbabbbbabaaaabbbbbaaababbaaaa"
      "aaaababbbaabbbbababbabab"
      "aabbbbbabbbaaaaaabbaababbbabbababaabaababaabbabb"
      "abbbbbaabababaaababbbaba"
      "baabbaaabbabbaaaaaaababa"
      "aabaabbbbabbbbaabbbbabbaaabaaabaabbaaabbaabaabbabbabaaaa"
      "abbbbbaabbabbbbaabbaaaab"
      "baabaabaababaaaaabbabaab"
      "babaabbbaaaaabbababbbbab"
      "aaaababbbbaabaaabbaaaababbbbbaab"
      "babaaaaaabbabbbaaaaabaababbbabab"
      "abababaabbbaaaaaaabbabaaaabbaaabaabbaabb"
      "aabaaabbbbbabbbbbbabbabb"
      "aaaaabbaaaaaaabbabbabababbbbbaaababbbaabbbabaabaabababab"
      "babbbbaabbabaaaaabbaabbbabaabbabbabaabbabaababbb"
      "babaaaaaabbabbbbbabbabab"
      "aaaababbbaabbbbababbabba"
      "bbaabbaabbaaabbbbbbaabbbaabbaaababaababaabaaaaabbaabaabaaababaab"
      "babbaaabbaaaabbbbbaabbabbaabaabbaaaababa"
      "babaaababbaababbabaabbaabbbabaabbbbababa"
      "aaabbbaaaabaabaabaaaaaaabaabaabaaababaaabbbabbbababbbbbbbabababb"
      "aabbabbbaabbbbbbabbaaaabbbabababaaaaabbb"
      "bbbbababbaabbaaabbababaabbbbbbbaaaaaaaba"
      "abaaaabbababbabbaabaaaaaabbaaababababbba"
      "bbbabbbbabbabbbaaaabbbabbbaaabaabaaabaabababaaabaaabaabb"
      "abbaabbbabbabbbabbaaabababbabaaaabbaaabb"
      "aabbbaaaabaaabaaaaaaababbabbabaaabaaaaaaabbaaaba"
      "abbaabbbaabaabbaaabbbabb"
      "abbaabbbbaaabaaababbbaab"
      "abaababaabbbbbaaabbaaaaabbabbbaabaabbaabbbbbbaabbaaaaaba"
      "abababaababaabbbbaaaabbb"
      "bbbaaaaabbaaababababbbbaaaababaa"
      "baaabbaabbbaabbbababbaaababbabaababbabba"
      "bbaabbaabaabababbbbbaabbaaababab"
      "bbaaaabbabaabbabbabaabaabaabbbbbaaabbabbbaabbbbabaaabbbabababbabbbabaabbbabbbaaa"
      "bbabbbaaaabaabbbabbbaababaaababbbababbbabaabbbaaaababaabbabbbbbbabaaaabbbabbbabbababaaba"
      "abaaabbbbababbbbaabbabbabbbbabaaaaaaabbababbbbab"
      "aaabbbabbbbbaabbabaababb"
      "aabbbbbbaabaaaaabbbbbaab"
      "baaababbbbbbaaaabbababaaaaaabbbababababa"
      "ababaaaabbaabbaaababbbbbbbabbabaaabaabbbbaabbabb"
      "bbaaaaababbaabbaaabaaabbbbbbbaab"
      "bbaababbabaaababaaaabbbabaaaabbabababbaaabbaaabb"
      "ababaaaaabbbbbababbbbaaa"
      "abbabbbbbbbaaabaababaabb"
      "bbababaaabbbabbaababbbbaaaabbaaaabbbaabb"
      "abaabbabaabaaaaaaabaababaabbbbbaaaabbaba"
      "aaaabaabbbbabbabbbaaaabbababaaba"
      "bbbbaaaababbbbaabababbaa"
      "bbababaabbababaabbaaaaaa"
      "baaabaaaababbbaaaaaababaaabaaabbbabbbaab"
      "abaabbaaaabaaaabbabbabbbaababbaabbabbabb"
      "aaababbababaaaaaabaaaaba"
      "baabbaaabbaaaabbbaaaaaaaabaaaabbbabbaabbabbaaaab"
      "aaabaabbbabababbababbbaaabaababb"
      "baabbbbaabbbaabbaabaabbb"
      "bbbbbbababbbbbaaaabbbabb"
      "bbbaaababbbaabaaaabababaaabaababbaaaabbaabbbbbbb"
      "baaabbaaaaaaaabbbbaabaaa"
      "abbabbbbaaabaababbaaabba"
      "baabababaabbaaabababaabb"
      "abbabababbbbabaabbaababaabbbabbbabaababa"
      "babaaaaababaaaabaabaabbb"
      "bbbbbaaabbabaaaabbaaaabbbbbaaababbbaababababababbababbaa"
      "baabbaaabbababaaaababaaabbbbbbabaaabbbbaaaaaaabbababaabbbbabbbaa"
      "bbaaabababbaaaaabbbaabab"
      "bbbbbbabaabbabbaabbbbaaa"
      "bbabaaaaaaababbaaababbaa"
      "aaababbaaabaaaaabababbba"
      "abaaaabbabbbabbaabbabbab"
      "bbaaababaaabaabaaababaabbbaabbbb"
      "bbbaabbbabbabaaaabbbaabaaabbbaabababbaabababaabbbbbbbabbbababaab"
      "aaabbbababbbbababaaaabaa"
      "bbabbababbaaabaaaaabbbabaaaababa"
      "abbaaaaaaabababaaabaabbaabbbbaaa"
      "bbabbbbaababbbbbbabaabbababababababbbbbb"
      "abbabaaabababaaababbbaaa"
      "babaaabaaabbbbaabaabbbbb"
      "abbaaaaaabbabbbbbabbabaa"
      "bbabaaaabababaaabaaabbab"
      "baabaabaabbababababaabbabaabbaaaababaabbaaaaaaab"
      "bbbaaaaabbabbbbabbbbbbba"
      "abbbaaaaabababbaabbaabbbaabbababaabbbbab"
      "aaabaaaabbbababbabbaaabbbabbabbbabbbaaba"
      "abaaaaabaaabbbaaaaaaaaab"
      "bbaaabbbbabaabbbbbbbabba"
      "bbaabbbaababbabababaabbbaaabababbbbababb"
      "bbbbaaaaaababaaaabaababa"
      "baabaabaaaabbbabbabaaabbbababbbbabbbaabbbbaababaababbbaabbabbbaa"
      "baaabbbbaabbabbbbbbbabaaabbbaaabbbbbabba"
      "bbaababbbbbaaaabbbbbbbabbababaaaaabababb"
      "baabaaaaabbabbaaaabbaaba"
      "aabaaaaaababbaaaabbaaabb"
      "aaabbbbabbbaaabbaabbaabb"
      "baaaaaaaababaaaababbbbbabbaabbaaaabbbbaabaabaabbaaabaaaaabaababb"
      "bababbbbabaababbaaaaaaaaabbabaabbaabbbbb"
      "babaaaabbbaaababbbabbaaaabbbbbbb"
      "bbababaaabbaaaaababbbbaabbababab"
      "baabaaaaaababbabaaaabbabaaaabbabaaabaabb"
      "baaaaaabbabaabaabbabaaaabaababaabbabaabbbbababaabaabbbbabbbaabba"
      "aabbabbbbaabbaaabbbababa"
      "aabbbbbbaabbbbaabbababaabbababaaaaaaabaaaaababaa"
      "aabbabbbabbabbbbbabbbabbbabbbbabbbabbbab"
      "abbaabaababaabbbababbabbabaaaaabaababbaa"
      "babaabbbabababaabbaabbbaaaabbaaabaaaababbaabbbbb"
      "ababbaaabababbbbabababab"
      "bbbaabaaaaaaaabbbbabbbaa"
      "aabbabbbaabbabaabababaaababaababbbbbbabb"
      "bbbbbbabaabbbbaaaabbaabb"
      "bbbbaabaababbbbbabaabbbaaabaaabaabbbbabb"
      "abababaaababaaaaaaaabbaa"
      "bbaabbbbabaabbabbbbabaabaaababbb"
      "babbbbaababbbabbbbaaaaabbbbababa"
      "bbaaaabbabbabababbbaabaaababbbbbbbaaabbbbabbabbbaabbabab"
      "abaabaaaababbbbaaaaaaaab"
      "abbababbabaaababbaabbabb"
      "aababbbaaabbbbbbbbabbaab"
      "bbbabbbaabaaaababbabbabbbaaaaababbabbbbaaabbabbbabaaaaaabbbabbab"
      "abaaaabaababaabaaaaabaaaaaaabbabbbabbabaaaababbbaaaaabbbbbbaaaababbabaabbaabbaaabbbbabbb"
      "baaababbbaabaabaabbbbbbb"
      "babbbababaaaaaaabbaaabbbbaaaababbabbbabbbabbbbbaaaabbabaabbbaaaaabaabbbbbaabbbab"
      "abbbaabbaababbbbabbbbaab"
      "baabaaaaabbaababbaaaabbb"
      "abababbaabbabbbaaaaabbba"
      "aabababababbbbbaabaababa"
      "babaaabbbbbbababaabaaaaaaaababaa"
      "babbbbaaaabaaabaaaaaaaaa"
      "bbbaaaaaababbabaaaaaaabbabbabbbaabaaabbbbbababbb"
      "abbbbbaabbaabbbbaabaaabaaaabbbabaabbabbaabababaababbbbbbbabaababbbabaaba"
      "aaabbbababbaababbbbbaabaaaababaa"
      "bababaaaabbabaaabaaabaaaaaabaaabababbbaa"
      "abbaabbabbaabbbaaaabbaba"
      "bbbabbabaabaabaabbabaaba"
      "abbbbbaabaabbbbabbbbbaaababbbbaaaaababaa"
      "bbbaaaabbbababaaaababbaaaabaabaaaabaabbaaababbaa"
      "baaaaaabaabbabbbbabbaaab"
      "aaaababbbbababaaabaababb"
      "aabaaabbaababbbaaaaaaaaa"
      "bbabbaaaaaaabbbbbbabaaab"
      "abaaababaaabbaaabaabaababbababaaaaabaababaabbbaababbbbab"
      "ababaabaaababaabbbabbabaababbbbbabbbabaaaaaaaabbbabbbbaa"
      "abaaababaabaaaabbaaaaaba"
      "ababaaaabbabbbbabababbbabaaabaab"
      "aabaaabbababbbbababbbaba"
      "baaaaaabababbabaabaaaaba"
      "baababbaababbabbabbbaaaabbbbbaaabaabaabb"
      "ababbbbbbaaabbbbbbbaabab"
      "bbabbaaabbabbaaaaaabaabb"
      "aaabbbaabbbbbaaabaaaabba"
      "abbbaaaaabbabbbaababbbbababaabbbbaaababbabaabaabbaaabbabbaaaababbbbbbaba"
      "bbaaabaaabaaabbbbbaabbbaaabbaaaa"
      "bbabbababbabbabaababbabaaababbbbabbabbaaaabbababaaaaabab"
      "abaaabbabbbbbaaabbabaabb"
      "aabbbababaaaaaaabbaabaaa"
      "bbbabbaaaabaaabbaabbbabb"
      "bbbaabbbabbbabaabbbbbabb"
      "abbbabaabbbaabaaabbbbbaaabbabababababbbabaabbabaaabababbbbbbbbbbbbbbabba"
      "bbbaaabbaabaaabaaaaaabbaaabaabababbaaaabaaabbbbb"
      "aaaaabaaaabbbbbaababbbbbabababbabbbbbaaaabaaabbaaaaabaaabbaaaaba"
      "abbbbababbbbabaabaaaabaa"
      "aabbabbbaaabbaaaababbaaabababbba"
      "aabaaabaabaaabbbbaababaa"
      "aaaabaabaabbbbbbbaaaaaabaaaaaaab"
      "abbaabaaaaaaabaaabbbabbaababbbbaabababbb"
      "baaaababaaaaaaabbaabaaab"
      "abaabbababaabaabaabbbabaaababbaa"
      "aaabbbabbaabbbbabbabbabaaaababaa"
      "aabaababbaaaaaabbbababab"
      "aaaaabaaabaabbabaaaaaaab"
      "aabbbbaabaababbaabbbbbba"
      "ababbabaabababaabbbabbbaababbbaaaaaaaaaaaaababaaaaabbaab"
      "bbbbbbababbaababbbbabbba"
      "bbaaabababbbabbabbbbaabababbaabb"
      "ababbbbabbbaaaaabbaaabbbaabaabaabbaaabaababbabab"
      "bbbaabaaaaabbbbaaaababbabbabbbaa"
      "abbabbaabaabaaaababbbaaa"
      "bbbabbabababbbbaaabaaabbbaababbbbbbbbaba"
      "abbaabbabbbbbaaababbbbaabbabaaaaabbbbbbb"
      "bbabbababbaaabbbabbbababbabbabbbbbaaaabaabbbbaababbabbbaabaaaaba"
      "abaabbbaaaaaabbbababaaaaababbbbbababbbba"
      "abbbaabbabbbaabbaabbbabb"
      "aabababaabbbabbabbaababaaabaaaaabaababaabaaabbba"
      "abaaabbbbbbbbaaaabbaabbbababbabbbbaabbaabaaaabaaaabbaaaabababbab"
      "aabbbabaaababaaabaaaabbb"
      "abaaaaabbaabbaaaaabbaabababbaaaa"
      "bbbaaaaababbbbaabaaaaabbabaabbbb"
      "babbbbbaaaabbbbaaaaababbaaabbabbababaaab"
      "bbaaabbbabaabaaaaabaaaabbbabbababbaaabbaaaababbb"
      "aabbbababaaaaaabaaaaaaab"
      "baaaaaabaaabaabaaaaabaabbaaabbab"
      "aabbbbbbaabababaaaaabbaa"
      "ababbbaaabaababbabaaabaa"
      "aabbabbabbbbbbbbabbbabbbababaabbaaababab"
      "aabbaaaaaabbabbabaababbaababaaaabbbabaaaaabbbbbbbabababa"
      "abbbaaaabbbaabbbaababbbaaaabbaaababbaaababaaaaaa"
      "aaaabababbbbabbaaaaaaaaababaaabbaaabaaba"
      "aaabbbaabbaaababbabbbbab"
      "aabababaaabbaaabbbbaaaababbaabbbaaabaaababaababa"
      "aabbabaaaaababbabbabbbbb"
      "bbbabbaabaaababababbabaa"
      "babbbbbabbaaabbbaababaaaaaaaaabaabbaaaba"
      "bbbaaabaababbabababaaaabbbbbbabbbbbbbabbaabbabaaababbbabababaaababbbbbabbbababbb"
      "abaabbaabaaabbbbabbbaaab"
      "aabaabbbabbbbbbaabbbbbbaaaababbbbabbbbbb"
      "bbbbaaabaabbbabbbaaaaaaababaaababbbabaaabbaabbbaaabababbabbbbabb"
      "aaaabaabbbaababbaaababab"
      "baaaaabbabaabbbbabbaaabababbabbbbaabaabbababaaabaaaaababaaaaabaabbbabbbbbbaaaaababaaabab"
      "babaaaabbaaababaaaababaa"
      "aaaaabbaabbaababaabbbbaaabaabaabaabbaaba"
      "aabbaaabaabaababbaababbaabbbaaabbabaabab"
      "ababaaaabababbbbaaababbaaabaabaa"
      "babaabbabaaabaaaabbbbaaa"
      "ababbbbabaaaaaabbbbaaaabbaaaababbbaaaaba"
      "bbaaababbaaabaaabaabbbab"
      "babaaaaabaaaaabbbaaaaaabbaabbbabbabababa"
      "baaabbbbabbabbbaabaaaaaa"
      "abbaabbaaababbbabaabbbab"
      "aaabbaaaabbbbabaaabaaaaabaabbaab"
      "baaaaabbababbbbabbaaaaabbbbbbaab"
      "bbaabbbbbbaababaaabbbaab"
      "aabaaabbbbaababbabbaababbbaaaaaa"
      "abaaaabaabbbbabbbaaaaabbbababbabbbaaaababbbbbbbb"
      "abaabbbabbaabbaabaabaababaabbaaaabbbbbba"
      "bbbabbbbbaabbaaababbbabbaabbaaba"
      "bababbbbabbbabbaabbaabaabbbabbabaaabababaaaabbba"
      "baabbaaaabbaabaaaabababaaabbbbbaabaabbabbabaabaaabbabbabbaaaaaba"
      "bbbbaabaabaabbabbbbaabaabbaaaaabaaabaaababababbabaaaabab"
      "abbaabaabbbaaababbabbbab"
      "aabaababbbababaabbbbbbabbaaaabaaabaabbbb"
      "abaabbbababaaabaabaaabababaaaaabbbababba"
      "baaaaabbbbbabbaaababaaaaabaabbabbbabbbaaaaabbababaabaaabbaaaaaaa"
      "aababbbbaabaabaabababaaaabababaaaababbbababbaabbbbbabaaabbababba"
      "bbaababbaaabbbabaaaabaababaaaabbbaaaabbb"
      "abbabaaaaaabbaaaabaabaab"
      "bbbbaabbbbbabaabbabaabbaaabbbbaababbbbaaaaaabaaaaaaaaaab"
      "bbbbaaaaababbaaaaaababbaaabaaaaabaabbaba"
      "bbbbabbbbbaabbaaabababbb"
      "aabaabaababbbabbbbbbaaab"
      "aaaabbbbbbbbbaaabaababbb"
      "bbbabbabbbbaaaabaaaababa"
      "ababbbabbbbbaaaaabbbbbba"
    ])

(def part2 (insta/parser source))

(->> messages
     (filter #(not (insta/failure? (insta/parse part2 %))))
     (count)
     (print))
