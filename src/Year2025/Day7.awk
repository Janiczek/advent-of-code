NR==1 { 
  sx = index("S",$0)
  beams[sx]
  times_got_to[1][sx] = 1
}
/^\.+$/ { times_got_to[NR] = times_got_to[NR-1] }
/\^/ { 
  new_beams = []
  for (bx in beams) {
    if ($0[bx] == "^") {
      p1++
      times_got_to[NR][bx-1] += times_got_to[NR-1][bx]
      times_got_to[NR][bx+1] += times_got_to[NR-1][bx]
      new_beams[bx-1]
      new_beams[bx+1]
    } else {
      times_got_to[NR][bx] += times_got_to[NR-1][bx]
      new_beams[bx]
    }
  }
  beams = new_beams
}
END { print(p1, sum(times_got_to[NR])) }
