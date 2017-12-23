def is_prime(n):
  if n == 2 or n == 3: return True
  if n < 2 or n%2 == 0: return False
  if n < 9: return True
  if n%3 == 0: return False
  r = int(n**0.5)
  f = 5
  while f <= r:
    if n%f == 0: return False
    if n%(f+2) == 0: return False
    f +=6
  return True    

b = 108100
c = 125100
step = 17
h = 1001
primes = 0

for current in range(b,c+1,step):
    if is_prime(current):
        primes += 1

print(h - primes)
