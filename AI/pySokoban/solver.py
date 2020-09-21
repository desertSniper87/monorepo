#solve sokoban

import sys, time
from typing import Tuple, List

import SokoMap, HashTable
import os

from settings import DEV_DEBUG

DIRECTION_DICT = {
    (1, 0): "Right",
    (0, 1): "Down",
    (-1, 0): "Left",
    (0, -1): "Up"
}

def getFormattedMoves(moveList) -> str:
    if len(moveList) == 0:
        return "Empty."
    else:
        stringMoveList = [DIRECTION_DICT[d] for d in moveList]
        return ' '.join(x for x in stringMoveList)



# Manhattan Distance between two points
def manhattanDistance(a, b):
    return abs(a[0]-b[0]) + abs(a[1]-b[1])

def heuristic(sokomap):
    # generate all possible combinations of goals for each block
    solutions = []
    for block in sokomap.getBlocks():
        usedSolution = []
        for goal in sokomap.getGoals():
            sol = (block, goal, manhattanDistance(block, goal))
            usedSolution.append(sol)
        solutions.append(usedSolution)

    # for sol in solutions:
    #     print sol
    # print "------"

    # Select the best
    best = sys.maxsize
    for s in solutions[0]:
        usedGoal = []
        usedBlock = []
        usedSolution = []

        usedGoal.append(s[1])
        usedBlock.append(s[0])
        usedSolution.append(s)
        manhttnGoalBlock = s[2]
        for solution in solutions:
            for col in solution:
                goal = col[1]
                block = col[0]
                if goal not in usedGoal and block not in usedBlock:
                    usedSolution.append(col)
                    usedGoal.append(col[1])
                    usedBlock.append(col[0])
                    manhttnGoalBlock = manhttnGoalBlock + col[2]
                    break
        if manhttnGoalBlock < best:
            best = manhttnGoalBlock
            result = usedSolution

    # print "-------"
    # print result
    # print best

    player = sokomap.getPlayer()
    currentEstimate = sys.maxsize
    v = (-1,-1)
    for block in sokomap.getUnplacedBlocks():
        if manhattanDistance(player, block) < currentEstimate:
            currentEstimate = manhattanDistance(player, block)
            v = block
    if v is not (-1,-1):
        best = best + currentEstimate

    return best

def isClosed(closedSet, x):
    for y in closedSet:
        if x == y:
            return True
    return False


def IDAstar(sokomap, heuristic) -> [(int, int)]:
    MAXNODES = 20000000
    openSet: [sokomap] = []
    closedSet: [sokomap] = []
    visitSet: [sokomap] = []
    pathLimit = heuristic(sokomap) - 1
    it = 0

    sokomap.uniqueBlocksGoals()
    sokomap.buildLowerBoundTable()

    while True:
        pathLimit += 1
        print("current pathLimit = ", pathLimit)
        sokomap.setG(0)
        openSet.insert(0, sokomap)
        ht = HashTable.HashTable()
        nodes = 0

        while len(openSet) > 0:
            currentState = openSet.pop(0)
            if DEV_DEBUG:
                currentState.printMap()
                moveList = currentState.getMoveList()
                print(f'"openSet.pop(0)" movelist={getFormattedMoves(moveList)}')

            if len(currentState.getUnplacedBlocks()) < 1:
                print()


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
                children = currentState.getChildren()
                for child in children:
                    # test if node has been "closed"
                    if isClosed(closedSet,child):
                        continue

                    # check if this has already been generated
                    if ht.checkAdd(child):
                        if DEV_DEBUG:
                            print(f'We have already gone this way.')
                        continue

                    # compute G for each
                    child.setG(currentState.getG() + 1)
                    child.setF(child.getG() + heuristic(child))
                    #x.setParent(currentState)

                    if DEV_DEBUG:
                        print(f'Moving.')
                    openSet.insert(0, child) # push
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
        for child in visitSet:
            if child.getF() < low:
                low = child.getF()
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

