from collections import defaultdict
from itertools import chain
if __name__ == '__main__':
    # f = open('input10004.txt')
    while(True):
        num_of_nodes = int(input())
        # num_of_nodes = int(f.readline())
        if num_of_nodes == 0: break
        graph = defaultdict(list)
        # num_of_edges = int(f.readline())
        num_of_edges = int(input())
        for edge in range(0, num_of_edges):
                i1, i2 = map(int, input().split())
                # i1, i2 = map(int, f.readline().split())
                graph[i1].append(i2)
                graph[i2].append(i1)
                # graph = {i1: i2}
                # graph = {'i2': [i1]}

        bicolor = True
        red = []
        blue = []
        for node in graph:
            if node not in chain(red, blue):
                red.append(node)
                blue.extend(graph[node])
            elif node in red:
                for neibor in graph[node]:
                    if neibor not in chain(red, blue):
                        blue.append(neibor)
                    elif neibor in red:
                        bicolor = False
            elif node in blue:
                for neibor in graph[node]:
                    if neibor not in chain(red, blue):
                        red.append(neibor)
                    elif neibor in blue:
                        bicolor = False

        if bicolor == True:
            print('BICOLORABLE.')
        else:
            print('NOT BICOLORABLE.')

exit(0)
        # print(graph)

# while True:
#     try:
#         line = input()
#         if line:
#             lines.append(line)
#             else:
#             break
#         except EOFError:
#         break
#     iput = '\n'.join(lines)
#
# print(''.join(output))
# exit(0)
# ii