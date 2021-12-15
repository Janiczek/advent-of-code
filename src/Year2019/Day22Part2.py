m = 119315717514047
a = 18870914573696
b = 115490606888493
n = 101741582076661

def compose(m,f,g):
    (a,b) = f
    (c,d) = g
    new_a = (a * c) % m
    new_b = ((a * d) % m + b) % m
    return (new_a, new_b)

def repeat(m,n,x):
    if n <= 0:
        return (1,0)
    y = (1,0)
    while n > 1:
        if n % 2 == 0:
            x = compose(m,x,x)
            n = n // 2
        else:
            y = compose(m,x,y)
            x = compose(m,x,x)
            n = (n - 1) // 2
    return compose(m,x,y)

#print(compose(10,(3,1),(3,1)))
#for n in range(1,10):
#    print(n,repeat(10,n,(3,1)))

print(repeat(m,n,(a,b)))
