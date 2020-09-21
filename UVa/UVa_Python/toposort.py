from collections import defaultdict

class Graph:
    def __init__(self, vertices):
        self.graph = defaultdict(list)
        self.V = vertices

    def addEdge (self, u, v):
        self.graph[u].append(v)

    def topoSortUtil(self, v, visited, stack):
        visited[v] = True

        for i in self.graph[v]:
            if visited[i]==False:
                self.topoSortUtil(i, visited, stack)

        stack.insert(0, v)

# def topologicalSort()