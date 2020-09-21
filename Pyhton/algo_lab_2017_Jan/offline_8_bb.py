from collections import defaultdict, OrderedDict
from itertools import combinations


"""Branch and bound approximation for vertex set cover problem"""

def greedy_approach(elem):
    """Here, we take a greedy approach from CSC787 WISC handout.

    We, take U . Formally, C is a set cover if S i ∈C S i = U . We would like to minimize |C|.
    Suppose we adopt the following greedy approach: In each step, choose the set S i containing the
    most uncovered points. Repeat until all points are covered.

    :elem: defaultdict(list)
    :returns: list, list

    """
    size = {}
    universe = []
    for i in elem:
        size[i] = len(elem[i])
        i = list(set().union(elem[i]))
        universe = list(set(universe).union(i))

    ordered_size = OrderedDict(sorted(size.items()))
    comb = []
     
    comb = combinations(ordered_size, 2)
    for i in comb:
        if list(set().union(elem[i[0]], elem[i[1]])) == universe:
            return elem[i[0]] , elem[i[1]]




if __name__ == '__main__':
    f = open('offline_4_input.txt')
    line = f.readline()
    l = line.rstrip().split(" ")
    n = int(l[0])
    m = int(l[1])
    i = 0
    elem = defaultdict(list)
    for line in f :
        l = line.rstrip().split(" ")
        l = [int(x) for x in l]
        elem[i] = l
        i = i+1

    a,b = greedy_approach(elem)
    print(a, b)
