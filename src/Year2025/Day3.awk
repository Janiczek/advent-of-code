{
  max_ihi = length($0) + 1

  [m1,i1] = find_max_digit($0,0, max_ihi-1)
  [m2,i2] = find_max_digit($0,i1,max_ihi)
  p1 += int(m1 m2)

  [n1,j1]   = find_max_digit($0,0,  max_ihi-11)
  [n2,j2]   = find_max_digit($0,j1, max_ihi-10)
  [n3,j3]   = find_max_digit($0,j2, max_ihi-9)
  [n4,j4]   = find_max_digit($0,j3, max_ihi-8)
  [n5,j5]   = find_max_digit($0,j4, max_ihi-7)
  [n6,j6]   = find_max_digit($0,j5, max_ihi-6)
  [n7,j7]   = find_max_digit($0,j6, max_ihi-5)
  [n8,j8]   = find_max_digit($0,j7, max_ihi-4)
  [n9,j9]   = find_max_digit($0,j8, max_ihi-3)
  [n10,j10] = find_max_digit($0,j9, max_ihi-2)
  [n11,j11] = find_max_digit($0,j10,max_ihi-1)
  [n12,j12] = find_max_digit($0,j11,max_ihi)
  p2 += int(n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12)
}
END {print(p1,p2)}

function find_max_digit(s,ilo,ihi) {
  max = -1
  max_i = -1
  for (i=ilo+1;i<ihi;i++) {
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
