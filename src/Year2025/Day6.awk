BEGIN { global digits; digits = ENVIRON["EXAMPLE"] == "1" ? 3 : 4 }
/[+*]/ { op_row = 1 }
op_row == 0 {
  input[NR] = $0
  for (i = 1; i <= NF; i++) {
    problem1[i][] = $i
  }
} 
op_row == 1 {
  cols = 0
  for (y = 1; y <= length($0); y++) {
    c = $0[y]
    if (c == "*" || c == "+") {
      cols++
      colstart[cols] = y
      if (cols > 1)
        colend[cols-1] = y-2
    }
  }
  colend[cols] = y-1
  # P2 post-parse processing
  for (col = 1; col <= NF; col++) {
    collength[col] = colend[col] - colstart[col] + 1
    is_left_aligned = range(1,NR-1)
        |> map((row) => { input[row][colstart[col]] })
        |> filter((c) => { c == " " })
        |> is_empty()
    pad = is_left_aligned ? pad_right : pad_left
    problem2[col] = problem1[col]
      |> map(pad)
      |> transpose()
      |> map((arr) => {
        arr
        |> gsub("Z","")
        |> int()
      })
      |> filter((x) => { x > 0 })
    x = problem1[col] |> map(pad)
    y = problem1[col] |> map(pad) |> transpose()
  }
  # Actual calculation
  for (col = 1; col <= NF; col++) {
    fn   = $col == "+" ? add : mul
    init = $col == "+" ? 0   : 1
    p1 += problem1[col] |> reduce(fn,init)
    p2 += problem2[col] |> reduce(fn,init)
  }
  print(p1, p2)
}
function add(a,b) {a+b}
function mul(a,b) {a*b}
function concat(a,b) {a b}
function pad_left(n)  { return repeat("Z",digits-length(str(n))) n }
function pad_right(n) { return n repeat("Z",digits-length(str(n))) }
function repeat(c,n) { n > 0 ? c repeat(c,n-1) : "" }
function is_empty(arr) { arr == [] }
function transpose(strs) {
  # Expects same length (col count) in all strings (rows)
  len = length(strs[1])
  return range(1,len)
    |> map((col) => {
      strs
        |> map((str) => {str[col]})
        |> reduce(concat, "")
    })
}
