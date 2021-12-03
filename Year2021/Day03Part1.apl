⍝in←'00100' '11110' '10110' '10111' '10101' '01111' '00111' '11100' '10000' '11001' '00010' '01010'
in←⊃⎕NGET'/Users/martinjaniczek/Localhost/elm/advent-of-code/Year2021/day03.txt'1
p1←×/2⊥¨{⍵(~⍵)}⌊0.5+(+⌿÷≢)⍤2↑⍎¨¨in
