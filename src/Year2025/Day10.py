from scipy.optimize import linprog
from itertools import chain, combinations
def powerset(s): return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))
def s(xs): return ",".join([str(x) for x in xs])
def ii(s): return [int(x) for x in s.split(",")]

p1 = p2 = 0
for line in open("input/2025/10.txt").readlines():
    parts = line.rstrip().split(" ")

    needed = set(i for i,c in enumerate(parts[0][1:-1]) if c == "#")
    btns = [ii(part[1:-1]) for part in parts[1:-1]]
    joltages = ii(parts[-1][1:-1])
    empty_result = [0 for i in range(len(joltages))] 

    for presses in powerset(range(len(btns))):
        result = set()
        for btn in presses:
            for pos in btns[btn]:
                if pos in result:
                    result.remove(pos)
                else:
                    result.add(pos)
        if result == needed:
            p1 += len(presses)
            break

    # P2: linear programming
    btns2 = [[0 for i in range(len(btns))] for j in range(len(joltages))]
    for i,btn in enumerate(btns):
        for j in btn:
            btns2[j][i] = 1

    result = linprog(c=[1] * len(btns), A_eq=btns2, b_eq=joltages, integrality=1)
    p2 += int(sum(result.x))

print("p1",p1)
print("p2",p2)
