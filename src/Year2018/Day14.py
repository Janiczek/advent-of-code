def step2(input, input_len, input0, input1, state, state_len, i1, i2):
    while True:
        n1 = state[i1]
        n2 = state[i2]
        new_n = n1 + n2
        new_n0 = new_n % 10
        new_n1 = new_n // 10

        if new_n0 == input0:
            if new_n1 == input1:
                if state[-(input_len - 2):] == input[:-2]:
                    return state_len - input_len + 2
            else:
                if state[-1] == input1:
                    if state[-(input_len - 1):] == input[:-1]:
                        return state_len - input_len + 1


        elif new_n > 9 and new_n1 == input0:
                if state[-(input_len - 1):] == input[:-1]:
                    return state_len - input_len + 1

        if new_n > 9:
            state.append(new_n1)
            state.append(new_n0)
            state_len += 2
        else:
            state.append(new_n0)
            state_len += 1

        i1 = (1 + n1 + i1) % state_len
        i2 = (1 + n2 + i2) % state_len


def go(str):
    lst = [int(c) for c in str]
    length = len(str)
    print(str, step2(lst, length, lst[-1], lst[-2], [3,7], 2, 0, 1))

go("51589")
go("01245")
go("92510")
go("59414")
go("503761")
