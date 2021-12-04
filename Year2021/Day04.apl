in←⊃⎕NGET'/Users/martinjaniczek/Localhost/elm/advent-of-code/Year2021/day04.txt'1
d←⍎¨','(≠⊆⊢)⊃in
n←⍴b←2⊃⎕VFI∊' '∘,¨1↓in
B←5 5∘⍴¨b⊂⍨n⍴1,24⍴0
p←(,¨,\)d
w←(∨/∧/∨∧⌿)¨↑{⍵∘(∊⍨)¨B}¨p
s←↑{(+/∊)¨⍵∘{⍵~⍺}¨¨B}¨p
p1←⊃0~⍨,d×[1]w∧s
p2←⊃0~⍨,d×[1]w∧s×[2]1=⍋⍒+⌿~w
