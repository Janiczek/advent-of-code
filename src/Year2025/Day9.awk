BEGIN { FS="," }
{ r[] = [int($1),int($2)] }
END {
  maxr = -1
  maxarea = -1
  rs = r |> sort()
  for (i = 1; i <= NR/2; i++) {
    for (j = NR; j >= NR/2; j--) {
      a = area(rs[i],rs[j])
      if (a > maxarea) {
        maxr = [rs[i],rs[j]]
        maxarea = a
      }
    }
  }
  print("p1",maxarea)
}
function area(p1,p2) { return (abs(p1[1]-p2[1])+1) * (abs(p1[2]-p2[2])+1) }
# TODO destructuring in function args
