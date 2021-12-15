in←⊃⎕NGET'/Users/martinjaniczek/Localhost/elm/advent-of-code/Year2021/day10.txt'1
o c←↓⍉↑s←'()' '[]' '{}' '<>'
r←({⍵/⍨∧⌿↑(⍵∘{~{(1@⍵)b}1+⍸b←⍵⍷⍺})¨s}⍣≡)
p1←+/{⊃0~⍨3 57 1197 25137 0[c⍳w/⍨~∧/o∊⍨w←r⍵]}¨in
p2←{⍵[⌈2÷⍨≢⍵]}{⍵[⍋⍵]}0~⍨{g×{⍺+5×⍵}/0,⍨o⍳w⊣g←∧/o∊⍨w←r⍵}¨in
