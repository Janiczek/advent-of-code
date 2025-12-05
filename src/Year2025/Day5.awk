BEGIN { FS = "-" }
/-/  { start[] = int($1); end[] = int($2) }
/^$/ { test_ids = 1; next }
test_ids == 1 {
  id = int($0)
  for (i in start)
    if (start[i] <= id && end[i] >= id) {
      p1++
      next
    }
} 
END { print(p1) }
