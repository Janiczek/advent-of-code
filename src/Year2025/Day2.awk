BEGIN { FS="-"; RS="," }
{ 
  seen = []
  [from,to] = [$1,$2] |> map(int)
  lento = length("" to)
  for (i=1;i<=60000;i++) {
    leni = length("" i)
    s = int(i i)
    lens = 2 * leni

    if (s >= from && s <= to) {
      sum1 += s
    }

    if (!(s in seen) && s >= from && s <= to) {
      sum2 += s
      print(s,from,to)
      seen[s] = 1
    }

    # i = 123    (len 3)
    # s = 123123 (len 6)
    # 3333333    (len 7)
    # 1231231    (fits 2x)
    while (lens+leni <= lento) { # can fit another one
      s = int(s i)
      lens += leni
      if (!(s in seen) && s >= from && s <= to) {
        sum2 += s
        seen[s] = 1
        print(s,from,to)
      }
    }
  }
}
END { print(sum1,sum2) }

