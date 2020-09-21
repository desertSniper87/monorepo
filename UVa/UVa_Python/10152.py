def find_position(string, order):  # (string, list)
    for i in range(len(order)):
        if order[i] == string:
            return order.index(string)


def yertle(initial_order, finishing_order):  # (list, list)
    result = []
    for i in range(len(finishing_order), 1, -1):
        current_position = find_position(initial_order[i-1], finishing_order)
        next_position = find_position(initial_order[i-2], finishing_order)
        if current_position<next_position:
            temp = initial_order[i-1]
            del initial_order[i-1]
            initial_order.insert(0, temp)
            #finishing_order.remove(temp)
            result.insert(0, temp)

    for i in finishing_order:
        for j in result:
            if i==j:
                temp = i
                result.remove(i)
                result.insert(0, temp)

    return result



if __name__ == '__main__':
    # f = open('input10152.txt')
    # numOfTests = int(f.readline())
    num_of_tests = int(input())

    for t in range(num_of_tests):
        num_of_lines = int(input())

        initial_order = []
        for line in range(num_of_lines):
            initial_order.append(input().rstrip())

        finishing_order = []
        for line in range(num_of_lines):
            finishing_order.append(input().rstrip())


        # print(initial_order)
        # print(finishing_order)

        result = yertle(initial_order, finishing_order)
        for i in result:
            print (i)

        print('\n')

    exit(0)
