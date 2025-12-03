{
  [m1,i1] = find_max_digit($0,0,length($0))
  [m2,i2] = find_max_digit($0,i1,length($0)+1)
  result = int(m1 m2)
  sum += result
}
END {print(sum)}

function find_max_digit(s,i_lower_bound,i_upper_bound) {
  max = -1
  max_i = -1
  for (i=i_lower_bound+1;i<i_upper_bound;i++) {
    n = int(substr(s,i,1))
    if (n > max) {
      max = n
      max_i = i
    }
  }
  return [max,max_i]
}

# LANGUAGE WISHLIST:
# sort()
# substr_index()
# counter/frequencies?
# string indexing ("abc"[2] == "b")
