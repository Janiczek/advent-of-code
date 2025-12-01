BEGIN { pos = 50 }
/L/ { m = match(/L([0-9]+)/, $0); pos = (pos - int(m[1])) % 100 }
/R/ { m = match(/R([0-9]+)/, $0); pos = (pos + int(m[1])) % 100 }
pos == 0 { pw++ }
END { print(pw) }
