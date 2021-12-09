in←↑⍎¨¨⊃⎕NGET'/Users/martinjaniczek/Localhost/elm/advent-of-code/Year2021/day09.txt'1
p1←+/∊z×({x←∊⍵⋄∧/(y[2×⍳4])>5⌷y←(⌈/x)@(0∘=)x}⌺3 3)z←1+in
