input_ = "463528179"
i = [int(c) for c in input_]
indices = [-1] + [i.index(n) for n in range(1,len(i)+1)]

# Part 2: make more elements
i += range(10,1_000_001)
indices += range(9,1_000_000)

# TODO why do we _not_ need to maintain the index as we move items around?

highest = max(i)

class Node:
    def __init__(self, next=None, val=None):
        self.next = next
        self.val = val

    def print_cycle(self):
        n = self.next
        l = [self.val]
        while n is not self:
            l.append(n.val)
            n = n.next
        print(l)

# Create the circular list
first_node = Node(None, i[0])
nodes = [first_node]
previous_node = first_node
for c in i[1:]:
    current_node = Node(None, c)
    nodes.append(current_node)
    previous_node.next = current_node
    previous_node = current_node
current_node.next = first_node


def find_destination_node(node):
    # figure out the wanted value
    unwanted_vals = [
        node.next.val,
        node.next.next.val,
        node.next.next.next.val,
    ]
    wanted_val = node.val - 1
    if wanted_val <= 0:
        wanted_val = highest
    while wanted_val in unwanted_vals:
        wanted_val -= 1
        if wanted_val <= 0:
            wanted_val = highest

    return nodes[indices[wanted_val]]
    
def swap_three_next_to(source_node, destination_node):
    node1 = source_node.next
    node3 = source_node.next.next.next
    node4 = source_node.next.next.next.next
    after_destination_node = destination_node.next

    source_node.next = node4
    destination_node.next = node1
    node3.next = after_destination_node


def run(n):
    step = n
    current_node = first_node
    while step > 0:
        destination_node = find_destination_node(current_node)
        swap_three_next_to(current_node, destination_node)
        current_node = current_node.next
        step -= 1

#run(100)
run(10_000_000)
node_with_1 = nodes[indices[1]]

#node_with_1.print_cycle() # Part 1
print(node_with_1.next.val * node_with_1.next.next.val) # Part 2
