BEGIN { FS="," }
{ r[] = [int($1),int($2)] }
END {
  maxr = -1
  maxarea = -1
  r = r |> sort()
  for (i = 1; i <= NR/2; i++) {
    [ax,ay] = r[i]
    for (j = NR; j >= NR/2; j--) {
      [bx,by] = r[j]
      a = area(ax,ay,bx,by)
      if (a > maxarea) {
        maxr = [[ax,ay],[bx,by]]
        maxarea = a
      }
    }
  }
  print(maxarea)
}
function area(ax,ay,bx,by) { return (abs(bx-ax)+1) * (abs(by-ay)+1) }
# TODO destructuring in function args
