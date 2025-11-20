# part 1: viable pairs of nodes

# any two nodes (A,B) regardless of whether they are directly connected, such that
# A.Used != 0 
# A != B
# A.Used <= B.Avail

NR == 3 { loop = 1 }
loop == 1 {
  match($1, /-x([0-9]+)-y([0-9]+)$/, coord)
  x = coord[1]
  y = coord[2]

  ++n
  coor[n][1] = x
  coor[n][2] = y
  used[x,y] = int($3)
  avai[x,y] = int($4)
}
END {
  for (i = 1; i <= n; i++) {
    x1 = coor[i][1]
    y1 = coor[i][2]
    used1 = used[x1,y1]
    if (used1 == 0) { continue }
    for (j = 1; j <= n; j++) {
      if (i == j) { continue }
      x2 = coor[j][1]
      y2 = coor[j][2]
      avai2 = avai[x2,y2]
      if (used1 <= avai2) { count++ }
    }
  }
  print count
}
