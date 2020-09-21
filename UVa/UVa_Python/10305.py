from collections import defaultdict

# def toposort(st, visited, stack):
#     visited[st] = True
#     for x in graph[st]:
#         if not visited[x]:
#             toposort(x, visited, stack)
#
#     stack.append(st)
#     print(stack)
    # return x, visited, stack
import sys


def toposort(st, visited, stack):
    # print(graph)
    visited[st] = True
    # print(graph[st])
    for x in graph[st]:
        if not visited[x]:
            toposort(x, visited, stack)

    if graph[st]==[]:
        stack.append(st)

        if len(stack2)!=0:
            stack.append(stack2.pop())

    elif all(visited[x]==True for x in graph[x]):
        stack2.append(st)

    # return stack
    # print(stack)
    # print(stack2)
    # return x, visited, stack

if __name__ == '__main__':
    # f = open('10305_3.txt')

    while True:
        parts = input().rstrip().split(" ")
        # parts = f.readline().rstrip().split(" ")
        if "" in parts:
            parts.remove("")
        # print(parts)
        num_of_t, num_of_rel = int(parts[0]), int(parts[1])
        if num_of_t==0 and num_of_rel==0:
            break

        graph = defaultdict(list)
        for i in range(1,num_of_t+1):
            graph[i] = []

        # print(graph)
        for i in range (0, num_of_rel):
            line = input().rstrip().split(" ")
            # line = f.readline().rstrip().split(" ")
            if "" in line:
                line.remove("")
            # print(line)
            graph[int(line[0])].append(int(line[1]))
            # print(graph)

        visited = {}
        for g in graph:
            visited[g] = False
        # print(visited)
        stack = []
        stack2 = []

        for g in graph:
            if not visited[g]:
                toposort(g, visited, stack)

        stack.extend(stack2)

        # print(stack2)
        stack.reverse()
        for x in stack:
            sys.stdout.write(x+" ")
        print()
        # graph, visited, stack, st = toposort(graph, visited, stack, 1)
        # print(stack)

        # input()


