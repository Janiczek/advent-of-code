BEGIN { pos = 50 }
/L/ { m = match(/L([0-9]+)/, $0); n = int(m[1]); npos = (pos - n) % 100;
      pw2 = pw2 + int(n/100);
      if (npos == 0 || (npos >= pos && pos != 0)) pw2++; pos = npos }
/R/ { m = match(/R([0-9]+)/, $0); n = int(m[1]); npos = (pos + n) % 100;
      pw2 = pw2 + int(n/100);
      if (npos == 0 || (npos <= pos && pos != 0)) pw2++; pos = npos }
pos == 0 { pw1++ }
END { print(pw1, pw2) }
