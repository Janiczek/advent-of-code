BEGIN { pos = 50 }
{ m = match(/[LR]([0-9]+)/, $0); n = int(m[1]); pw2 = pw2 + int(n/100) }
/L/ { npos = (pos - n) % 100; if (npos == 0 || (npos >= pos && pos != 0)) pw2++ }
/R/ { npos = (pos + n) % 100; if (npos == 0 || (npos <= pos && pos != 0)) pw2++ }
{ pos = npos }
pos == 0 { pw1++ }
END { print(pw1, pw2) }
