if __name__ == '__main__':
    f = open('offline_6_b1_input.txt')
    num_of_tests = int(f.readline())
    for _ in range(num_of_tests):
        n = int(f.readline())
        coin_value_dict = {}
        coin_value_list = []
        line = f.readline().rstrip().split(" ")
        for i in range(n):
            coin_value_list.append(int(line[i]))

        coin_value_list = sorted(coin_value_list, reverse=True)
        change = int(f.readline().rstrip())
        for i in coin_value_list:
            coin_value_dict[i] = 0
            while i<=change:
                coin_value_dict[i] = coin_value_dict[i] + 1
                change = change - i

        for key in coin_value_dict:
            print(key," ", coin_value_dict[key])
