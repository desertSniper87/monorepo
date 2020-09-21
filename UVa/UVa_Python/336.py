from collections import defaultdict

import re


def remove_values_from_list(the_list, val):
    return [value for value in the_list if value != val]


if __name__ == '__main__':
    while (True):
        f = open('input336.txt')
        num_of_edges = int(f.readline())
        if num_of_edges == 0:
            break
        graph = defaultdict(list)
        for _ in range(0, num_of_edges):
            line = f.readline().rstrip()
            # print(line)
            parts = line.split(" ")
            # print(parts)
            values = remove_values_from_list(parts, "")
            # print(values)
            it = iter(values)
            v = zip(it, it)
            for i1, i2 in v:
                graph[i1].append(i2)
                graph[i2].append(i1)
            # print(graph)

            line = f.readline()
            li = remove_values_from_list(line)
            tup = [tuple(map(int, i.split(' '))) for i in ]
            print(tup)

            if start_edge == '0' and ttl == '0':
                break
