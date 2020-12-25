#card_pub_key = 5764801
#door_pub_key = 17807724

card_pub_key = 14788856
door_pub_key = 19316454

def step(n,subject_number):
    return (n * subject_number) % 20201227

card_loop_size = 0
card_current = 1
while card_current != card_pub_key:
    card_loop_size += 1
    card_current = step(card_current, 7)

door_loop_size = 0
door_current = 1
while door_current != door_pub_key:
    door_loop_size += 1
    door_current = step(door_current, 7)

i = 0
current = 1
while i < card_loop_size:
    i += 1
    current = step(current, door_pub_key)

print(current)
