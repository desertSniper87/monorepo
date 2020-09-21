#solve sokoban

import sys, time
import SokoMap, HashTable
import os

# Manhattan Distance between two points
def manDistance(a, b):
    return abs(a[0]-b[0]) + abs(a[1]-b[1])

def heuristic(sm):
    # generate all possible combinations of goals for each block
    solutions = []
    for b in sm.getBlocks():
        solution = []
        for g in sm.getGoals():
            sol = (b, g, manDistance(b,g))
            solution.append(sol)
        solutions.append(solution)

    # for sol in solutions:
    #     print sol
    # print "------"

    # Select the best
    best = sys.maxsize
    for s in solutions[0]:
        usedGoal = []
        usedBlock = []
        solution = []

        usedGoal.append(s[1])
        usedBlock.append(s[0])
        solution.append(s)
        h = s[2]
        for lin in solutions:
            for col in lin:
                if col[1] not in usedGoal and col[0] not in usedBlock:
                    solution.append(col)
                    usedGoal.append(col[1])
                    usedBlock.append(col[0])
                    h = h + col[2]
                    break
        if h < best:
            best = h
            result = solution

    # print "-------"
    # print result
    # print best

    w = sm.getPlayer()
    d = sys.maxsize
    v = (-1,-1)
    for x in sm.getUnplacedBlocks():
        if manDistance(w, x) < d:
            d = manDistance(w, x)
            v = x
    if v is not (-1,-1):
        best = best + d

    return best

def isClosed(closedSet, x):
    for y in closedSet:
        if x == y:
            return True
    return False


def IDAstar(sm, h):
    MAXNODES = 20000000
    openSet = []
    closedSet = []
    visitSet = []
    pathLimit = h(sm) - 1
    sucess = False
    it = 0

    while True:
        pathLimit = pathLimit + 1
        print("current pathLimit = ", pathLimit)
        sm.setG(0)
        openSet.insert(0, sm)
        ht = HashTable.HashTable()
        nodes = 0

        while len(openSet) > 0:
            currentState = openSet.pop(0)
            #currentState.printMap()

            nodes = nodes + 1
            if currentState.isSolution():
                return currentState # SOLUTION FOUND!!!

            if nodes % 1000000 == 0:
                print((nodes/1000000), "M nodes checked")
            if nodes == MAXNODES:
                print("Limit of nodes reached: exiting without a solution.")
                sys.exit(1)

            if currentState.getF() <= pathLimit:
                closedSet.insert(0, currentState)
                # get the sucessors of the current state
                for x in currentState.children():
                    # test if node has been "closed"
                    if isClosed(closedSet,x):
                        continue

                    # check if this has already been generated
                    if ht.checkAdd(x):
                        continue

                    # compute G for each
                    x.setG(currentState.getG() + 1)
                    x.setF(x.getG()+ h(x))
                    #x.setParent(currentState)
                    openSet.insert(0, x) # push
            else:
                visitSet.insert(0, currentState)

        #print "Nodes checked = ", nodes
        print("iteration = ", it)
        it = it + 1
        if len(visitSet) == 0:
            print("FAIL")
            return None

        # set a new cut-off value (pathLimit)
        low = visitSet[0].getF()
        for x in visitSet:
            if x.getF() < low:
                low = x.getF()
        pathLimit = low

        # move nodes from VISIT to OPEN and reset closedSet
        openSet.extend(visitSet)
        visitSet = []
        closedSet = []


def depth_first_search__scan(sm, h):
    MAXNODES = 20000000
    openSet = [sm]
    ht = HashTable.HashTable()
    ht.checkAdd(sm)
    nodes = 0

    while len(openSet) > 0:
        currentState = openSet.pop()
        #currentState.printMap()

        nodes += 1
        if currentState.isSolution():
            return currentState # SOLUTION FOUND!!!

        if nodes % 1000 == 0:
            print(nodes, " nodes checked")
            sys.stdout.flush()
        if nodes == MAXNODES:
            print("Limit of nodes reached: exiting without a solution.")
            sys.exit(1)

        for x in currentState.children():
            # check if this has already been generated
            if ht.checkAdd(x):
                continue

            openSet.append(x)
    return None

if __name__ == '__main__':

    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("filename", type=str,
            help="a path to the filename with the board.")
    parser.add_argument("--method", type=str, default="IDAstar",
            help="The method - \"dfs\" or \"IDAstar\"")
    args = parser.parse_args()

    smap = SokoMap.SokoMap()

    smap.readMap(args.filename)

    smap.printMap()
    print("-----")
    smap.staticDeadlock()
    print("-----")
    smap.printMap()
    #sys.exit(1)

    #smap.buildInfluenceTable()
    #sys.exit(-1)


    start = time.time()
    # TODO : Implement using a command line arg instead of the environment
    # variable
    scan_function = IDAstar
    if args.method == 'dfs':
        scan_function = depth_first_search__scan
    elif args.method == 'IDAstar':
        scan_function = IDAstar
    else:
        print("Unknown scan type")
        sys.exit(-1)

    sol = scan_function(smap, heuristic)
    print(time.time()-start)
    if sol is not None:
        sol.printMap()
        print("\n")
        print(sol.getMoveList())

