BEGIN {global max_x,max_y}
{ 
  y = NR
  for (x = 1; x <= length($0); x++) {
    c = substr($0,x,1)
    grid[x,y] = c
  }
}
END {
  max_x = x-1
  max_y = y
  for (y=1;y<=max_y;y++) {
    for (x=1;x<=max_x;x++) {
      c = grid[x,y]
      #p = c == "@" ? (p1_ok(x,y,grid) ? "x" : "@") : "."
      #printf(p)
      if (p1_ok(x,y,grid)) p1++
    }
    #print()
  }
  print("p1:",p1)
}
function p1_ok(x,y,grid) {
  if (grid[x,y] == ".") return 0
  ns = [[x-1,y-1],[x,y-1],[x+1,y-1],
        [x-1,y],          [x+1,y],
        [x-1,y+1],[x,y+1],[x+1,y+1]]
  for (nk in ns) {
    [nx,ny] = ns[nk]
    if (nx >= 1 && nx <= max_x
     && ny >= 1 && ny <= max_y
     && grid[nx,ny] == "@") p++
  }
  return p < 4
}
