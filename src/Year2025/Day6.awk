/[+*]/ { op_row = 1 }
op_row == 0 {
  problem[i] = []
  for (i = 1; i <= NF; i++) {
    problem[i][] = $i
  }
} 
op_row == 1 {
  for (col = 1; col <= NF;   col++) {
    fn   = $col == "+" ? add : mul
    init = $col == "+" ? 0   : 1
    p1 += problem[col] |> reduce(fn,init)
  }
  print(p1)
}
function add(a,b) {a+b}
function mul(a,b) {a*b}
