BEGIN {
  FS = ","
  N = ENVIRON["EXAMPLE"] == "1" ? 10 : 1000
}
{ boxes[$1,$2,$3] }
END { 
  len = length(boxes)
  ks = keys(boxes)
  mst = []

  seen_edges = []
  for (nn = 1; nn <= N; nn++) {
    print("N",nn,"/",N)
    mindist = 10^9
    mink1 = -1
    mink2 = -1
    for (i = 1; i < len; i++) {
      k1 = ks[i]
      if (!(k1 in mst)) mst[k1] = []
      [x1,y1,z1] = split(SUBSEP, k1)
      for (j = i+1; j <= len; j++) {
        k2 = ks[j]
        if (!(k2 in mst)) mst[k2] = []
        [x2,y2,z2] = split(SUBSEP, k2)
        [s1,s2] = [k1,k2] |> sort()
        if (k2 in mst[k1] || k1 in mst[k2]) continue
        d = dist(x1,y1,z1, x2,y2,z2)
        if (d < mindist) {
          mindist = d
          mink1 = k1
          mink2 = k2
        }
      }
    }
    mst[mink1][mink2]
    mst[mink2][mink1]
  }
  print(p1(mst))
}
function dist(x1,y1,z1, x2,y2,z2) {
  return sqrt((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2)
}
function p1(mst) {
  return mst
    |> circuits()
    |> map(length)
    |> sort((x) => -x)
    |> take(3)
    |> product()
}
function circuits(mst) {
  mst2 = mst
  cs = []
  while (mst2 != []) {
    for (v in mst2) break
    [c,mst2] = circuit(v,mst2)
    cs[] = c
  }
  return cs
}
function circuit(v,mst) {
  mst2 = mst
  c[v]
  todos = mst2[v]
  delete mst2[v]
  while (todos != []) {
    for (todo in todos) break
    delete todos[todo]
    if (!(todo in mst2)) continue
    for (v2 in mst2[todo]) {
      if (!(v2 in c)) {
        c[v2]
        todos[v2]
      }
    }
    delete mst2[todo]
  }
  return [c,mst2]
}
function take(n,arr) {
  r = []
  for (i in arr) {
    r[i] = arr[i]
    if (++c == n) return r
  }
  return r
}
function product(arr) { return arr |> reduce((a,b) => a*b, 1) }

# TODO: some kind of CoW bug when doing nested array-set modifications inside a function argument - it leaks to the caller
# TODO: print SUBSEP in the string repr (and array string repr)
# TODO: bug with nested arrays (arr[1][2])
  # workaround: conn[min_key_1] = []; conn[min_key_1][min_key_2]
  # instead of the wanted: conn[min_key_1][min_key_2]
# TODO: don't add to sets when accessing them
# TODO: reverse fn
# TODO: for(;;) needs to support ,
# TODO: take fn
# TODO: product fn
# TODO: join function
