import numpy as np
from scipy.sparse import dok_array
from scipy.sparse.csgraph import minimum_spanning_tree

nodes = []
for line in open("input/2025/8.txt").readlines():
    [x,y,z] = [int(a) for a in line.rstrip().split(",")]
    nodes.append((x,y,z))

length = len(nodes)
adj = dok_array((length,length),dtype=np.float32)
for i in range(length-1):
    for j in range(i+1,length):
        (x1,y1,z1) = nodes[i]
        (x2,y2,z2) = nodes[j]
        adj[i,j] = ((x1-x2)**2 + (y1-y2)**2 + (z1-z2)**2)**0.5
        adj[j,i] = ((x1-x2)**2 + (y1-y2)**2 + (z1-z2)**2)**0.5

st = minimum_spanning_tree(adj)
xs,ys = st.nonzero()
edges = [(int(x),int(y),float(d)) for x,y,d in zip(xs,ys,st[xs,ys])]
s = sorted(edges,key=lambda t: t[2])
v1,v2,d = s[-1]
x1 = nodes[v1][0]
x2 = nodes[v2][0]
print(x1*x2)
