in←2⊃','⎕VFI⊃⊃⎕NGET'/Users/martinjaniczek/Localhost/elm/advent-of-code/Year2021/day07.txt'1
p1←{⌊/+⌿⍵∘.(|-)¯1+⍳1+⌈/⍵}in
p2←{⌊/+⌿⍵∘.{2!1+|⍵-⍺}¯1+⍳1+⌈/⍵}in
