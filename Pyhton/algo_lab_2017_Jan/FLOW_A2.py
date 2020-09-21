from collections import defaultdict

import sys
import operator


def kruskal_sbmst(weights, num_of_vert, cons):
    mst = defaultdict(dict)
    visited = {}
    sum = 0
    for i in range(num_of_vert):
        visited[i] = False
    # print(visited)

    for i in cons:
        # print(weights[0][0][0], " ", weights[0][0][1])
        print (i)
        # mst[i[0]][i[1]] = weights(i[0],i[1])
        visited[i[0]] = True
        visited[i[1]] = True

    for x in weights:
        # print(x)
        # print (x[0])
        # print(weights[x])
        # print(x[0][0]," ", x[0][1])
        # print(visited[x[0][0]], " ", visited[x[0][1]])
        # print(x[1])
        if visited[x[0][0]] == False or visited[x[0][1]] == False:
            # print("accepted")
            mst[x[0][0]][x[0][1]] = x[1]
            sum = sum + x[1]
            visited[x[0][0]] = True
            visited[x[0][1]] = True

    # print(mst)

    return mst, sum

if __name__ == '__main__':
    # f = open('offline_1_input_1.txt')
    f = open('FLOW_A2_INPUT.txt')

    num_of_vert = int(f.readline())
    num_of_edge = int(f.readline())

    graph = defaultdict(dict)
    for i in range(0, num_of_vert):
        graph[i] = {}

    for _ in range(0, num_of_edge):
        line = f.readline().rstrip().split(" ")
        graph[int(line[0])][int(line[1])] = int(line[2])
        graph[int(line[1])][int(line[0])] = int(line[2])

    num_of_cons = int(f.readline())
    cons = []
    for _ in range(num_of_cons):
        line = f.readline().rstrip().split(" ")
        line.remove('')
        cons.append((int(line[0]), int(line[1])))
        
    # print(cons)
    weights = {}
    
    # print(graph)
    for a in graph:
        for b in graph[a]:
            weights[a, b] = graph[a][b]

    weights = sorted(weights.items(), key = operator.itemgetter(1), reverse=False)
    # print(weights)

    # mst = defaultdict(dict)
    # visited = {}
    # for i in range(num_of_vert):
        # visited[i] = False
    # # print(visited)

    # for x in weights:
        # # print(x)
        # # print (x[0])
        # # print(weights[x])
        # # print(x[0][0]," ", x[0][1])
        # # print(visited[x[0][0]], " ", visited[x[0][1]])
        # # print(x[1])
        # if visited[x[0][0]] == False or visited[x[0][1]] == False:
            # # print("accepted")
            # mst[x[0][0]][x[0][1]] = x[1]
            # visited[x[0][0]] = True
            # visited[x[0][1]] = True

    for i in range(num_of_edge):
        mst, s = kruskal_sbmst(weights, num_of_vert, cons)
        # print(mst)
        # print(s)
        weights.pop(0)


    # print(weights)

        




