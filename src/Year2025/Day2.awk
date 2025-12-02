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
      seen[s] = 1
    }

    while (lens+leni <= lento) {
      s = int(s i)
      lens += leni
      if (!(s in seen) && s >= from && s <= to) {
        sum2 += s
        seen[s] = 1
      }
    }
  }
}
END { print(sum1,sum2) }

