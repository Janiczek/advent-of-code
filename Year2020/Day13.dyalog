exampleInput←7 13 59 31 19
exampleTs←939

part1←{⍵[tss⍳mints]×⍺-⍨mints←⌊/tss←⍵×⌈⍺÷⍵}

exampleTs part1 exampleInput

realInput←⊃(//','⎕VFI'41,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,971,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17,13,x,x,x,x,23,x,x,x,x,x,29,x,487,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19')
realTs←1000299

realTs part1 realInput
