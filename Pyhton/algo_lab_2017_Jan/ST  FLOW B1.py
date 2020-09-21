from collections import defaultdict

import sys


if __name__ == '__main__':
    f = open('ST  FLOW A1.input.txt')
    test_cases = f.readline()

    parts = f.readline().rstrip().split(" ")
    if "" in parts:
        parts.remove("")
    # print(parts)
    num_of_vert, num_of_edge = int(parts[0]), int(parts[1])

    graph = defaultdict(dict)
    for i in range(1, num_of_vert + 1):
        graph[i] = {}

    for _ in range(0, num_of_edge):
        # line = f.readline().rstrip().split(" ")
        graph[int(line[0])][int(line[1])] = int(line[2])
        graph[int(line[1])][int(line[0])] = int(line[2])


