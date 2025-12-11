# part 2

import graphlib

es = {}
esr = {}
vi = 0
for line in open("input/2025/11.txt").readlines():
    fields = line.rstrip().split(" ")
    src = fields[0][:-1]
    dests = fields[1:]
    es[src] = []
    for dest in dests:
        if dest not in esr:
            esr[dest] = []
        es[src].append(dest)
        esr[dest].append(src)

ts = graphlib.TopologicalSorter(esr)
toposorted = list(ts.static_order())

path_to_from = {}
for v_to in toposorted:
    if v_to not in path_to_from:
        path_to_from[v_to] = set()
    path_to_from[v_to].add(v_to)
    for v_from in esr.get(v_to,[]):
        path_to_from[v_to] |= path_to_from[v_from]

paths_to_fft = path_to_from["fft"]
paths_to_dac = path_to_from["dac"]

# DP: count number of paths based on number of parents' paths
counts = {"svr": 1}
for v in toposorted:
    if v not in es: continue

    # must be connected to fft (as source or as destination)
    # must be connected to dac (as source or as destination)
    paths_to_v = path_to_from[v]
    if not (v in paths_to_fft or "fft" in paths_to_v) \
    or not (v in paths_to_dac or "dac" in paths_to_v):
        counts[v] = 0
        continue

    for e in es[v]:
        if e not in counts: counts[e] = 0
        counts[e] += counts.get(v,0)

print(counts["out"])
