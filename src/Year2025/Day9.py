def area(xy1,xy2):
    return (abs(xy1[0]-xy2[0])+1)*(abs(xy1[1]-xy2[1])+1)

def flatten(xss):
    return [x for xs in xss for x in xs]

reds = {}
ordered = []
for line in open("input/2025/9.txt").readlines():
    x,y = [int(x) for x in line.rstrip().split(",")]
    if x not in reds: reds[x] = set()
    reds[x].add(y)
    ordered.append((x,y))

reds_sorted = sorted(flatten([[(x,y) for y in ys]
                              for x,ys in reds.items()]))

ls_xx = []
ls_yy = []
for (x1,y1),(x2,y2) in zip(ordered,(ordered[1:]+[ordered[0]])):
    if x1 == x2: ls_xx.append((x1,y1,y2))
    else:        ls_yy.append((y1,x1,x2))

def red_inside(p1,p2):
    x1,y1 = p1
    x2,y2 = p2
    y1,y2 = sorted([y1,y2])
    for (lx,ly1,ly2) in ls_xx:
        if lx == x1 or lx == x2:
            if ly1 < y1 and ly2 > y1: return True
            if ly2 < y1 and ly1 > y1: return True
            if ly1 < y2 and ly2 > y2: return True
            if ly2 < y2 and ly1 > y2: return True
        elif lx > x1 and lx < x2:
            if ly1 <= y1 and ly2 >= y1: return True
            if ly2 <= y1 and ly1 >= y1: return True
            if ly1 <= y2 and ly2 >= y2: return True
            if ly2 <= y2 and ly1 >= y2: return True
    for (ly,lx1,lx2) in ls_yy:                   
        if ly == x1 or ly == x2:
            if lx1 < x1 and lx2 > x1: return True
            if lx2 < x1 and lx1 > x1: return True
            if lx1 < x2 and lx2 > x2: return True
            if lx2 < x2 and lx1 > x2: return True
        elif ly > y1 and ly < y2:
            if lx1 <= x1 and lx2 >= x1: return True
            if lx2 <= x1 and lx1 >= x1: return True
            if lx1 <= x2 and lx2 >= x2: return True
            if lx2 <= x2 and lx1 >= x2: return True
    return False


maxarea = -1
maxp1 = maxp2 = None
length = len(reds_sorted)
for i,p1 in enumerate(reds_sorted):
    print(i+1,"/",length)
    for p2 in reversed(reds_sorted):
        if p2 <= p1:
            break
        a = area(p1,p2)
        if red_inside(p1,p2):
            continue
        if a > maxarea:
            maxarea = a
            maxp1 = p1
            maxp2 = p2
print("p2",maxarea)
