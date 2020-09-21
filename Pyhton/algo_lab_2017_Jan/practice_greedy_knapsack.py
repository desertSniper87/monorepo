if __name__ == '__main__':
    f = open('practice_greedy_knapsack_input.txt')
    num_of_tests = int(f.readline())
    for _ in range(num_of_tests):
        n = int(f.readline())
        # print(num_of_tests, n)
        weight = {}
        value = {}
        vpw = {}
        result = {}
        line = f.readline().rstrip().split(" ")
        for i in range(n):
            weight[i] = int(line[i])

        line = f.readline().rstrip().split(" ")
        for i in range(n):
            value[i] = int(line[i])

        for i in range (n):
            vpw[i] = value[i]/weight[i]

        max_weight = int(f.readline())
        print(vpw)

        for i in sorted(vpw, reverse=False):
            if (max_weight>0):
                print("max_weight", max_weight)
                print("weight[i]", weight[i])
                print("value[i]", value[i])
                # w = weight[i] % max_weight
                if weight[i] > max_weight:
                    w = weight[i] - max_weight;
                else:
                    w = weight[i]
                print("w", w)
                max_weight = max_weight - w
                result[i] = w
                print("max_weight", max_weight)
            # print (vpw[i])
            print(result)
            


        # print(weight)

