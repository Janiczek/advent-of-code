in←⍎¨⊃⎕NGET'/Users/martinjaniczek/Localhost/elm/advent-of-code/Year2021/day02.txt'1
m←{⍵='d':0j1⋄⍵='u':0j¯1⋄1}
p1←×/9 11○+/(⊃¨in){(m⍺)×+/⍎¨(∩∘⎕D)¨⍵}⌸in
