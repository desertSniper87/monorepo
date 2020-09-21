import sys
from copy import deepcopy

from typing import TypeVar
SokoMap = TypeVar('SokoMap')

from solver import DIRECTION_DICT, manhattanDistance

from settings import DEV_DEBUG

class SokoMap:
    TILE_BLOCK = '$'
    TILE_GOAL = '.'
    TILE_PLAYER = '@'
    TILE_SPACE = ' '
    TILE_WALL = '#'
    TILE_BLOCK_ON_GOAL = '*'
    TILE_PLAYER_ON_GOAL = '!'
    TILE_DEADLOCK = 'x'
    TILE_PLAYER_ON_DEADLOCK = '+'

    TILES_WRONG_FOR_BLOCK = frozenset([TILE_WALL, TILE_BLOCK_ON_GOAL, TILE_DEADLOCK]) # And Tile Block
    TILES_WRONG_FOR_2x2 = frozenset([TILE_WALL, TILE_BLOCK_ON_GOAL]) # And Tile Block
    TILES_SPACEY = frozenset([TILE_SPACE, TILE_DEADLOCK])

    lowerBoundTable = {}

    def isTileWrongForBlock(self, tile) -> bool:
        if self.isTileBlock(tile) or tile in self.TILES_WRONG_FOR_2x2:
            return True
        else:
            return False

    def isTileWrongFor2x2(self, tile) -> bool:
        if self.isTileBlock(tile) or tile in self.TILES_WRONG_FOR_BLOCK:
            return True
        else:
            return False

    def isTileBlock(self, tile) -> bool:
        if tile[0] == self.TILE_BLOCK:
            return True
        else:
            return False

    def isTileGoal(self, tile) -> bool:
        if tile[0] == self.TILE_GOAL:
            return True
        else:
            return False

    def isTileBlocky(self, tile) -> bool:
        if self.isTileBlock(tile) or tile == self.TILE_BLOCK_ON_GOAL:
            return True
        else:
            return False

    def __init__(self):
        self.sokomap = [] # Todo: Write a getter

        self.gVal = 0
        self.fVal = 0

        self.parent = None
        self.moveList = []

        self.lowerBoundSum = 0

    def __eq__(self, other):
        if isinstance(other, SokoMap):
            return (self.sokomap == other.sokomap and self.player == other.player)
        return NotImplemented

    def setPlayerOnMap(self, map, playerPosition):
        (x,y) = playerPosition
        self.sokomap = map
        self.player = (x,y)

    def setG(self, val):
        self.gVal = val

    def getG(self):
        return self.gVal

    def setF(self, val):
        self.fVal = val

    def getF(self):
        return self.gVal

    def setParent(self, parent):
        self.parent = parent

    def readMap(self, fileName):
        # read from txt map file
        mapFile = open(fileName, 'r')
        temp = mapFile.readlines()
        SokoMap = [] # in memory map representation: list of lists of chars
        y = 0
        player = None
        for line in temp:
            base_l = list(line)[0:-1] # removes newline
            l = []
            for x in range(0,len(base_l)):
                c = base_l[x]
                if c == self.TILE_PLAYER:
                    c = self.TILE_SPACE
                    player = (x,y)
                elif c == self.TILE_PLAYER_ON_GOAL:
                    c = self.TILE_GOAL
                    player = (x,y)
                elif c == self.TILE_PLAYER_ON_DEADLOCK:
                    c = self.TILE_DEADLOCK
                    player = (x,y)
                l.append(c)
            SokoMap.append(l)
            y += 1

        while SokoMap[-1] == ['\n'] or SokoMap[-1] == []:
             # remove last "line" (empty, original only had a newline)
            SokoMap.pop()

        self.setPlayerOnMap(SokoMap, player)

    def getMap(self):
        return self.sokomap

    def printMap(self, playerPosition=None):
        #y = len(self.sm)
        #x = len(self.sm[0])

        if playerPosition:
            (playerX, playerY) = playerPosition
        else:
            (playerX, playerY) = self.getPlayer()

        sokomap = deepcopy(self.sokomap)
        sokomap[playerY][playerX] = '@'

        for line in sokomap:
            print(''.join (x for x in line))
            # for c in line:
            #     print c,
            # print

        print(f'player_position={self.getPlayer()}')

    def getPositionOfBlock(self, block: str) -> []:
        result = []
        y = 0
        for line in self.sokomap:
            x = 0
            for i in line:
                if i == block or \
                block == self.TILE_BLOCK and self.isTileBlock(i) == True or\
                block == self.TILE_GOAL and self.isTileGoal(i) == True:
                    result.append((x,y))
                x += 1
            y += 1

        return result

    def getPositionOfBlocks(self, blocks):
        total = []
        for block in blocks:
            total.extend(self.getPositionOfBlock(block))
        return total

    def getGoals(self):
        return self.getPositionOfBlocks([self.TILE_GOAL, self.TILE_BLOCK_ON_GOAL])

    def getBlocks(self):
        return self.getPositionOfBlocks([self.TILE_BLOCK, self.TILE_BLOCK_ON_GOAL])

    def getUnplacedBlocks(self):
        return self.getPositionOfBlock(self.TILE_BLOCK)

    def getPlayer(self):
        return self.player

    def getWalls(self):
        return self.getPositionOfBlock(self.TILE_WALL)

    def getDeadlocks(self):
        return self.getPositionOfBlock(self.TILE_DEADLOCK)

    # def simplifyMap(self, block, goal):
    #     simpleMap = self.sm
    #
    #     for x in self.getBlocks() and x != block:
    #         simpleMap[x[1]][x[0]] = self.TILE_SPACE
    #     for x in self.getGoals() and x != goal:
    #         simpleMap[x[1]][x[0]] = self.TILE_SPACE
    #
    #     m = SokoMap()
    #     m.setPlayerOnMap(simpleMap)
    #     return m

    def isLegal(self, nplayer) -> bool:
        (nx, ny) = nplayer
        (x, y) = self.getPlayer()

        #self.printMap()


        if nx < 0 or ny < 0 or ny >= len(self.sokomap) or nx >= len(self.sokomap[ny]):
            return False

        if self.sokomap[ny][nx] == self.TILE_WALL:
            return False # cant move into a wall

        if self.isTileBlocky(self.sokomap[ny][nx]):
            # is trying to push a block
            # the only way this works is if the space after the block is free
            # or a goal so we calculate where the block is going to be pushed
            # into and see if it's "open"

            xdiff = nx - x
            ydiff = ny - y

            bx = nx + xdiff
            by = ny + ydiff


            # print "x,y=",x,y
            # print "nx,ny=",nx,ny
            # print self.sm[ny][nx]
            # print "bx,by=",bx, by
            # print self.sm[by][bx]

            if self.isTileWrongForBlock(self.sokomap[by][bx]):
                return False

            min_n_off = (-ydiff,-xdiff)

            # Check for a segment of 2*2 adjacent blocks or walls. Like:
            #
            #      $$    #$     #$   $$
            #      ##    #$     $$   $$
            #
            def _my_check(_dir_offsets):
                ((dy1,dx1),(dy2,dx2)) = _dir_offsets
                filtered = [self.TILE_BLOCK_ON_GOAL if self.isTileGoal(self.sokomap[by][bx]) else self.TILE_BLOCK]
                for (dy,dx) in [(dy1,dx1),(dy2,dx2),(dy1+dy2,dx1+dx2)]:
                    x = self.sokomap[by + dy][bx + dx]
                    if self.isTileWrongFor2x2(x):
                        filtered.append(x)
                # There are 4 in total
                return (len(filtered) == 4 and (self.TILE_BLOCK in filtered))

            for y_offset in [(-1,0),(1,0)]:
                if ((y_offset != min_n_off) and (not (y_offset == (-1,0) and by == 0 ))):
                    for x_offset in [(0,-1),(0,1)]:
                        if ((x_offset != min_n_off) and (not (x_offset == -1 and bx == 0 ))):
                            for dir_offsets in [
                                    (x_offset,y_offset),
                                    (y_offset,x_offset)
                                    ]:
                                if (_my_check(dir_offsets)):
                                    return False

        # everything is OK
        return True

    def isSolution(self):
        return (len(self.getUnplacedBlocks()) == 0)

    def tunnelMacro(self, nMap, box, push):
        (px, py) = push
        (bx, by) = box

        if px != 0:
            # horizontal push
            while nMap[by+1][bx] == self.TILE_WALL and nMap[by-1][bx] == self.TILE_WALL:
                if nMap[by][bx+1] != self.TILE_SPACE:
                    return None
                bx = bx + 1
        if py != 0:
            # vertical push
            while nMap[by][bx+1] == self.TILE_WALL and nMap[by][bx-1] == self.TILE_WALL:
                if nMap[by+1][bx] != self.TILE_SPACE:
                    return None
                by = by + 1

        if (bx, by) != box:
            # Some puzzles have multiple tunnels and require a box to pushed
            # into the tunnel and then the player to travel through another
            # tunnel and push it out of the tunnel again - credit: AJ
            # To solve that, the macro will not push the box out of the tunnel
            # but rather leave it on the edge
            bx -= px
            by -= py

            return (bx, by)

        return None

    def addMove(self, m):
        self.moveList.append(m)

    def setMoveList(self, l):
        self.moveList = deepcopy(l)

    def getMoveList(self):
        return self.moveList


    def move(self, target) -> SokoMap:
        (currentX,currentY) = self.getPlayer()
        map = deepcopy(self.sokomap)
        box = None

        # transform the new location of the player

        (targetX, targetY) = target
        xdiff = targetX - currentX
        ydiff = targetY - currentY
        move = (xdiff, ydiff)
        carry = False
        if self.isTileBlock(map[targetY][targetX]) == True:
            tileBlock = map[targetY][targetX]
            carry = True
            map[targetY][targetX] = self.TILE_SPACE
        elif map[targetY][targetX] == self.TILE_BLOCK_ON_GOAL:
            carry = True
            map[targetY][targetX] = self.TILE_GOAL

        # push a block into a new space if necessary
        if carry:
            bx = targetX + xdiff
            by = targetY + ydiff

            # box = self.tunnelMacro(map, (bx, by), move)
            box = None
            if box is not None:
                if DEV_DEBUG:
                    print ("TUNNEL")
                    self.printMap()
                    print("-------")
                    print("Tunnel From ", (bx, by), " to ", box)

                (bx, by) = box

                targetX = bx - xdiff
                targetY = by - ydiff
                # it must be a space (that's checked inside tunnelMacro)

                map[targetY][targetX] = self.TILE_SPACE

            # print ("")
            # print (bx,by)
            # for line in map:
            #     print (line)

            # Place the box
            if map[by][bx] == self.TILE_SPACE:
                map[by][bx] = tileBlock
            elif self.isTileGoal(map[by][bx]) == True:
                map[by][bx] = self.TILE_BLOCK_ON_GOAL
            else:
                print("WTF2=", map[by][bx])


        nSokoMap = SokoMap()
        nSokoMap.setPlayerOnMap(map, (targetX, targetY))
        nSokoMap.setMoveList(self.getMoveList())

        if DEV_DEBUG and box:
            print(f'Adding tunnel move {DIRECTION_DICT.get(move)}')

        nSokoMap.addMove(move)

        # if box is not None:
        #     nSokoMap.printMap()
        return nSokoMap

    def _filter_neighbours(self, player_position: tuple, offsets, filter: callable([object, tuple])):
        (x,y) = player_position
        for (dx,dy) in offsets:
            nxy = (x+dx,y+dy)
            if (filter(nxy)):
                yield nxy
                if DEV_DEBUG:
                    print(f'{DIRECTION_DICT.get((dx, dy))}{nxy} is Legal')

    def getChildren(self):
        return [self.move(nxy) for nxy in self._filter_neighbours(self.getPlayer(),
                                                                  [(0,-1),(0,1),(-1,0),(1,0)],
                                                                  (lambda nxy: self.isLegal(nxy)))]

    def getNeighbors(self, node):
        """Only used in creating influencer tables.

        """
        def filt(xxx_todo_changeme1):
            (nx,ny) = xxx_todo_changeme1
            try:
                return ny >= 0 and nx >= 0 and self.sokomap[ny][nx] != self.TILE_WALL
            except IndexError:
                return False

        return self._filter_neighbours(node, [(1,0),(-1,0),(0,1),(0,-1)], filt)

    def shortestPath(self, source, target):
        """Dijkstra's algorithm from the pseudocode in wikipedia

        Only used in creating influencer tables.
        """
        dist = {}
        prev = {}
        q = []
        for y,a in enumerate(self.sokomap):
             for x,b in enumerate(self.sokomap[y]):
                 dist[(x,y)] = sys.maxsize
                 prev[(x,y)] = None
                 q.append((x,y))
        dist[source] = 0

        while len(q) is not 0:
            # find the node with minimum value (u)
            d = deepcopy(dist)
            while True:
                b = dict([(item[1],item[0]) for item in list(d.items())])
                u = b[min(b.keys())]
                if u not in q:
                    d.pop(u)
                else:
                    break

            if dist[u] == sys.maxsize: # remaining nodes are inaccessible
                break

            q.remove(u)


            if u == target: # target found
                break

            for v in self.getNeighbors(u):
                alt = dist[u] + 1
                if alt < dist[v]:
                    dist[v] = alt
                    prev[v] = u

        s = []
        u = target
        while prev[u] is not None:
            s.append(u)
            u = prev[u]
        s.reverse()

        return s

    def uniqueBlocksGoals(self):
        blocki = 0
        goali = 0
        for line in self.sokomap:
            for i, tile in enumerate(line):
                if tile == self.TILE_BLOCK:
                    line[i] = self.TILE_BLOCK + str(blocki)
                    blocki += 1
                elif tile == self.TILE_GOAL:
                    line[i] = self.TILE_GOAL + str(goali)
                    goali += 1


        return




    def buildInfluenceTable(self):
        self.influenceTable = {}
        for sy,a in enumerate(self.sokomap):
            for sx,b in enumerate(self.sokomap[sy]):
                inf = {}
                if self.sokomap[sy][sx] == self.TILE_WALL:
                    break
                for ty,a in enumerate(self.sokomap):
                    for tx,b in enumerate(self.sokomap[ty]):
                        if self.sokomap[ty][tx] == self.TILE_WALL:
                            break
                        path = self.shortestPath((sx, sy), (tx, ty))
                        score = 0.0
                        for s in path:
                            (x, y) = s
                            sscore = 0.0
                            # Alternatives
                            for n in self.getNeighbors(s):
                                if n not in path:
                                    (nx, ny) = n
                                    px = nx - x
                                    py = ny - y
                                    hx = x - px
                                    hy = y - py
                                    if self.sokomap[ny][nx] == self.TILE_WALL:
                                        sscore = 0
                                    elif self.sokomap[hy][hx] != self.TILE_WALL:
                                        # "pushability" test
                                        # this tests if we can push a box from
                                        # s to n. It's an inacurate test
                                        # i.e. it's not always possible with
                                        # this condition but it will do
                                        sscore = 2
                                    else:
                                        sscore = 1
                            # Goal-Skew
                            for g in self.getGoals():
                                gpath = self.shortestPath((sx,sy), g)
                                if s in gpath:
                                    sscore /= 2
                                    break

                            # Connection
                            si = path.index(s)
                            if len(path) < si+1:
                                n = path[si+1]
                                (nx, ny) = n
                                px = nx - x
                                py = ny - y
                                hx = x - px
                                hy = y - py
                                # Same poor test for "pushability" as before
                                if self.sokomap[hy][hx] != self.TILE_WALL:
                                    sscore += 2
                                else:
                                    sscore += 1

                            # Tunnel
                            if si > 0:
                                (mx, my) = path[si-1]
                                px = x - mx
                                py = y - my

                                if px != 0: # horizontal push
                                    if self.sokomap[my + 1][mx] == self.TILE_WALL and \
                                       self.sokomap[my - 1][mx] == self.TILE_WALL:
                                        sscore = 0
                                else:   # vertical push
                                    if self.sokomap[my][mx + 1] == self.TILE_WALL and \
                                       self.sokomap[my][mx - 1] == self.TILE_WALL:
                                        sscore = 0

                            score += sscore
                        inf[(tx, ty)] = score
                self.influenceTable[(sx, sy)] = deepcopy(inf)

        average = 0.0
        count = 0

        for k,v in self.influenceTable:
            for kk, vv in v:
                count += 1
                average += vv

        average /= count
        if average < 6:
            self.influenceThresh = 6
        else:
            self.influenceThresh = average

        self.influenceHistory = 10

    class DirectView:
        def __init__(self, sm):
            self.sm = sm

        def get(self, xxx_todo_changeme2):
            (y, x) = xxx_todo_changeme2
            return self.sm[y][x]

        def set(self, xxx_todo_changeme3, val):
            (y, x) = xxx_todo_changeme3
            self.sm[y][x] = val
            return

        def y_len(self):
            return len(self.sm)

        def x_len(self):
            return max([row.len for row in self.sm])

    class Proxy_View:
        def __init__(self, v):
            self.v = v

        def _map(self, p):
            return p

        def get(self, p):
            return self.v.get(self._map(p))

        def set(self, p, val):
            return self.v.set(self._map(p), val)

        def y_len(self):
            return self.v.y_len()

        def x_len(self):
            return self.v.x_len()

    class Swap_XY_View(Proxy_View):
        def _map(self, xxx_todo_changeme4):
            (y, x) = xxx_todo_changeme4
            return (x,y)

        def y_len(self):
            return self.v.x_len()

        def x_len(self):
            return self.v.y_len()

    def staticDeadlock(self):
        """Detects fixed deadlocks (very basic, not perfect"""

        def _place_deadlock(y,x,delta_y,delta_x):
            try:
                if self.sokomap[y + delta_y][x] == self.TILE_WALL and \
                   self.sokomap[y][x + delta_x] == self.TILE_WALL:
                    self.sokomap[y][x] = self.TILE_DEADLOCK
                    return True
            except IndexError:
                pass
            return False

        # Place Deadlock Markers in corners (without goals)
        for y,a in enumerate(self.sokomap):
            for x,b in enumerate(self.sokomap[y]):
                if x == 0 or x == (len(self.sokomap[0]) - 1) or \
                   y == 0 or (y == len(self.sokomap) - 1):
                    continue
                if self.sokomap[y][x] == self.TILE_SPACE:
                    _place_deadlock(y,x,-1,-1) or \
                    _place_deadlock(y,x,-1,1) or \
                    _place_deadlock(y,x,1,-1) or \
                    _place_deadlock(y,x,1,1)

        # Connect Deadlock Markers if they next to a contin. wall w/o goals
        def connect_markers(dx,dy, view):
            up = True
            down = True
            found = False
            x = dx

            while x > 1:
                x -= 1
                try:
                    if view.get((dy,x)) == self.TILE_DEADLOCK:
                        found = True
                        break
                except IndexError:
                    break

            if found:
                sx = x
                while x != dx:
                    x += 1
                    try:
                        if view.get((dy+1,x)) != self.TILE_WALL and down:
                            down = False
                    except IndexError:
                        down = False
                    try:
                        if view.get((dy-1,x)) != self.TILE_WALL and up:
                            up = False
                    except IndexError:
                        up = False
                    try:
                        if not view.get((dy,x)) in self.TILES_SPACEY:
                            up = down = False
                    except IndexError:
                        down = up = False

                if up or down:
                    x = sx
                    while x != dx:
                        val = view.get((dy,x))
                        if val == self.TILE_SPACE:
                            view.set((dy,x), self.TILE_DEADLOCK)
                        x += 1

        xy_v = self.DirectView(self.sokomap)
        yx_v = self.Swap_XY_View(xy_v)
        for dead in self.getDeadlocks():
            (dx,dy) = dead
            connect_markers(dx, dy, xy_v)
            connect_markers(dy, dx, yx_v)

    def buildLowerBoundTable(self) -> None:
        """ This is a better lower bound estimator (well, routines that implements
        it). It uses minimum flow, maximum matching to solve which stone goes
        to which goal without conflicts. If no matching exists - deadlock!
        called after distances where changed to make sure we get the
        stones_done * onto their squares """

        # char stonei,goali;
        # int  taken[MAXSTONES];

        # initialize table with just any matching
        self.lowerBoundSum = 0
        self.pen = 0
        self.lowerBoundTable = {}
        # memset(taken,0,sizeof(int)*maze->number_stones);

        # first get all the done stones in
        nUnplacedBlocks = len(self.getUnplacedBlocks())
        for blockI, goalI in zip(self.getUnplacedBlocks(), self.getGoals()):
            print(blockI, goalI)
            self.lowerBoundSum += manhattanDistance(blockI, goalI)

        return


