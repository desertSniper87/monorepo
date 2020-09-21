from collections import defaultdict

import sys


def bellman_ford(graph, source, num_of_vert, num_of_edge):
    inf = float("inf")
    d = {}
    pi = {}
    for v in graph:
        d[v] = inf
        pi[v] = None
    d[source] = 0

    for _ in range(num_of_vert-2):
        for u in graph:
            for v in graph[u]:
                # relax(u, v, graph[u][v])
                if d[v] > d[u] + graph[u][v]:
                    d[v] = d[u] + graph[u][v]
                    pi[v] = u

    total_d = 0

    print('{:>12}  {:>12}  {:>12}'.format('V', 'Distance', 'Parent'))
    for i in graph:
        print('{:>12}  {:>12}  {:>12}'.format(str(i), str(d[i]), str(pi[i])))
        total_d = total_d + d[i]

    print("Total distance is", total_d)
    print()
    
    for u in graph:
        for v in graph[u]:
            if d[v] >  d[u] + graph[u][v]:
                return False

    return True


if __name__ == '__main__':
    #f = open('offline_2_input.txt')
    f = open('offline_2_input_2.txt')

    num_of_tests = int(f.readline())
    index_start = int(f.readline())

    while(num_of_tests):
        num_of_vert = int(f.readline())
        num_of_edge = int(f.readline())

        graph = defaultdict(dict)
        for i in range(index_start, num_of_vert + index_start):
            graph[i] = {}

        for _ in range(0, num_of_edge):
            line = f.readline().rstrip().split(" ")
            graph[int(line[0])][int(line[1])] = float(line[2])

        # print(graph)

        for u in graph:
            for v in graph[u]:
                print(u, " ", v, " ", graph[u][v])
        print()

        print("Source?")
        source = int(input())

        bellman_ford(graph, source, num_of_vert, num_of_edge) 

        for _ in range(num_of_edge):
            #min_edge = -9999
            min_edge = 9999
            for u in graph:
                for v in graph[u]:
                    # if graph[u][v] > min_edge:
                    if graph[u][v] < min_edge:
                        min_edge = graph[u][v]
                        min_u = u
                        min_v = v
            print("Popping ", min_u, " -->", min_v, " | ", min_edge)
            graph[min_u].pop(min_v)

            bellman_ford(graph, source, num_of_vert, num_of_edge) 
        

        num_of_tests = num_of_tests - 1

    print("End of program")

    
