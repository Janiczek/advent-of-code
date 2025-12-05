BEGIN { FS = "-" }
/-/ {
  todo_lo[] = int($1)
  todo_hi[] = int($2)
}
/^$/ { test_ids = 1; next }
test_ids == 1 { to_test[] = int($0) } 
END {
  while (length(todo_lo) > 0) {
    new_todo_lo = []
    new_todo_hi = []
    for (ti in todo_lo) {
      lo = todo_lo[ti]
      hi = todo_hi[ti]
      skip_adding = 0
      for (i in start) {
        s = start[i]
        e = end[i]
        if (lo >= s && hi <= e) skip_adding = 1
        else if (lo < s && hi >= s) { # overlap from left
          new_todo_lo[] = lo
          new_todo_hi[] = max(lo,s-1)
          skip_adding = 1

          if (hi > e) { # overlap also from right
            new_todo_lo[] = min(hi,e+1)
            new_todo_hi[] = hi
          }
        }
        else if (lo >= s && lo <= e && hi > e) { # overlap from right
          new_todo_lo[] = max(lo,min(hi,e+1))
          new_todo_hi[] = hi
          skip_adding = 1
        }
      }
      if (!skip_adding) {
        start[] = lo
        end[] = hi
        p2 += hi-lo+1
      }
    }
    todo_lo = new_todo_lo
    todo_hi = new_todo_hi
  }
  for (j in to_test) {
    id = to_test[j]
    for (i in start) {
      if (start[i] <= id && end[i] >= id) {
        p1++
        break
      }
    }
  }
  print(p1, p2)
}
