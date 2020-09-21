import sys
from copy import deepcopy

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

    TILES_BLOCKY = frozenset([TILE_BLOCK, TILE_BLOCK_ON_GOAL])
    TILES_WRONG_FOR_BLOCK = frozenset([TILE_BLOCK, TILE_WALL, TILE_BLOCK_ON_GOAL, TILE_DEADLOCK])
    TILES_WRONG_FOR_2x2 = frozenset([TILE_BLOCK, TILE_WALL, TILE_BLOCK_ON_GOAL])
    TILES_SPACEY = frozenset([TILE_SPACE, TILE_DEADLOCK])

    def __init__(self):
        self.sm = []

        self.gVal = 0
        self.fVal = 0

        self.parent = None
        self.moveList = []

    def __eq__(self, other):
        if isinstance(other, SokoMap):
            return (self.sm == other.sm and self.player == other.player)
        return NotImplemented

    def setMap(self, m, xxx_todo_changeme5):
        (x,y) = xxx_todo_changeme5
        self.sm = m
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

        self.setMap(SokoMap, player)

    def getMap(self):
        return self.sm

    def printMap(self):
        #y = len(self.sm)
        #x = len(self.sm[0])

        for line in self.sm:
            print(''.join(x for x in line))
            # for c in line:
            #     print c,
            # print

    def getSomething(self, something):
        result = []
        y = 0
        for l in self.sm:
            x = 0
            for i in l:
                if i == something:
                    result.append((x,y))
                x += 1
            y += 1

        return result

    def _getSeveralThings(self, somethings):
        total = []
        for thing in somethings:
            total.extend(self.getSomething(thing))
        return total

    def getGoals(self):
        return self._getSeveralThings([self.TILE_GOAL, self.TILE_BLOCK_ON_GOAL])

    def getBlocks(self):
        return self._getSeveralThings([self.TILE_BLOCK, self.TILE_BLOCK_ON_GOAL])

    def getUnplacedBlocks(self):
        return self.getSomething(self.TILE_BLOCK)

    def getPlayer(self):
        return self.player

    def getWalls(self):
        return self.getSomething(self.TILE_WALL)

    def getDeadlocks(self):
        return self.getSomething(self.TILE_DEADLOCK)

    # def simplifyMap(self, block, goal):
    #     simpleMap = self.sm
    #
    #     for x in self.getBlocks() and x != block:
    #         simpleMap[x[1]][x[0]] = self.TILE_SPACE
    #     for x in self.getGoals() and x != goal:
    #         simpleMap[x[1]][x[0]] = self.TILE_SPACE
    #
    #     m = SokoMap()
    #     m.setMap(simpleMap)
    #     return m

    def isLegal(self, nplayer):
        (nx, ny) = nplayer
        (x, y) = self.getPlayer()

        #self.printMap()


        if nx < 0 or ny < 0 or ny >= len(self.sm) or nx >= len(self.sm[ny]):
            return False

        if self.sm[ny][nx] == self.TILE_WALL:
            return False # cant move into a wall

        if self.sm[ny][nx] in self.TILES_BLOCKY:
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

            if self.sm[by][bx] in self.TILES_WRONG_FOR_BLOCK:
                return False

            min_n_off = (-ydiff,-xdiff)

            # Check for a segment of 2*2 adjacent blocks or walls. Like:
            #
            #      $$    #$     #$   $$
            #      ##    #$     $$   $$
            #
            def _my_check(xxx_todo_changeme):
                ((dy1,dx1),(dy2,dx2)) = xxx_todo_changeme
                filtered = [self.TILE_BLOCK_ON_GOAL if self.sm[by][bx] == self.TILE_GOAL \
                                             else self.TILE_BLOCK]
                for (dy,dx) in [(dy1,dx1),(dy2,dx2),(dy1+dy2,dx1+dx2)]:
                    x = self.sm[by+dy][bx+dx]
                    if x in self.TILES_WRONG_FOR_2x2:
                        filtered.append(x)
                # There are 4 in total
                return (len(filtered) == 4 and (self.TILE_BLOCK in filtered))

            for y_offset in [(-1,0),(1,0)]:
                if ((y_offset != min_n_off) and (not (y_offset == (-1,0) and by == 0 ))):
                    for x_offset in [(0,-1),(0,1)]:
                        if ((x_offset != min_n_off) and (not (x_offset == -1 and bx == 0 ))):
                            for dir_offsets in [\
                                    (x_offset,y_offset), \
                                    (y_offset,x_offset) \
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


    def move(self, nplayer):
        (x,y) = self.getPlayer()
        nMap = deepcopy(self.sm)
        box = None

        # transform the new location of the player

        (nx,ny) = nplayer
        xdiff = nx - x
        ydiff = ny - y
        m = (xdiff, ydiff)
        carry = False
        if nMap[ny][nx] == self.TILE_BLOCK:
            carry = True
            nMap[ny][nx] = self.TILE_SPACE
        elif nMap[ny][nx] == self.TILE_BLOCK_ON_GOAL:
            carry = True
            nMap[ny][nx] = self.TILE_GOAL

        # push a block into a new space if necessary
        if carry:
            bx = nx + xdiff
            by = ny + ydiff

            box = self.tunnelMacro(nMap, (bx, by), m)
            #box = None
            if box is not None:
                # print "TUNNEL"
                #  self.printMap()
                #  print "-------"
                # print "Tunnel From ", (bx, by), " to ", box
                (bx, by) = box

                nx = bx - xdiff
                ny = by - ydiff
                # it must be a space (that's checked inside tunnelMacro)

                nMap[ny][nx] = self.TILE_SPACE

            # print ""
            # print bx,by
            # for line in nMap:
            #     print line

            # Place the box
            if nMap[by][bx] == self.TILE_SPACE:
                nMap[by][bx] = self.TILE_BLOCK
            elif nMap[by][bx] == self.TILE_GOAL:
                nMap[by][bx] = self.TILE_BLOCK_ON_GOAL
            else:
                print("WTF2=", nMap[by][bx])


        nSokoMap = SokoMap()
        nSokoMap.setMap(nMap, (nx, ny))
        nSokoMap.setMoveList(self.getMoveList())
        nSokoMap.addMove(m)

        # if box is not None:
        #     nSokoMap.printMap()
        return nSokoMap

    def _filter_neighbours(self, xxx_todo_changeme6, offsets, filt):
        (x,y) = xxx_todo_changeme6
        for (dx,dy) in offsets:
            nxy = (x+dx,y+dy)
            if (filt(nxy)):
                yield nxy

    def children(self):
        return [self.move(nxy) for nxy in self._filter_neighbours(self.getPlayer(), [(0,-1),(0,1),(-1,0),(1,0)], (lambda nxy: self.isLegal(nxy)))]

    def getNeighbors(self, node):
        def filt(xxx_todo_changeme1):
            (nx,ny) = xxx_todo_changeme1
            try:
                return ny >= 0 and nx >= 0 and self.sm[ny][nx] != self.TILE_WALL
            except IndexError:
                return False

        return self._filter_neighbours(node, [(1,0),(-1,0),(0,1),(0,-1)], filt)

    def shortestPath(self, source, target):
        """Dijkstra's algorithm from the pseudocode in wikipedia"""
        dist = {}
        prev = {}
        q = []
        for y,a in enumerate(self.sm):
             for x,b in enumerate(self.sm[y]):
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


    def buildInfluenceTable(self):
        self.influenceTable = {}
        for sy,a in enumerate(self.sm):
            for sx,b in enumerate(self.sm[sy]):
                inf = {}
                if self.sm[sy][sx] == self.TILE_WALL:
                    break
                for ty,a in enumerate(self.sm):
                    for tx,b in enumerate(self.sm[ty]):
                        if self.sm[ty][tx] == self.TILE_WALL:
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
                                    if self.sm[ny][nx] == self.TILE_WALL:
                                        sscore = 0
                                    elif self.sm[hy][hx] != self.TILE_WALL:
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
                                if self.sm[hy][hx] != self.TILE_WALL:
                                    sscore += 2
                                else:
                                    sscore += 1

                            # Tunnel
                            if si > 0:
                                (mx, my) = path[si-1]
                                px = x - mx
                                py = y - my

                                if px != 0: # horizontal push
                                    if self.sm[my+1][mx] == self.TILE_WALL and \
                                       self.sm[my-1][mx] == self.TILE_WALL:
                                        sscore = 0
                                else:   # vertical push
                                    if self.sm[my][mx+1] == self.TILE_WALL and \
                                       self.sm[my][mx-1] == self.TILE_WALL:
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
                if self.sm[y+delta_y][x] == self.TILE_WALL and \
                   self.sm[y][x+delta_x] == self.TILE_WALL:
                    self.sm[y][x] = self.TILE_DEADLOCK
                    return True
            except IndexError:
                pass
            return False

        # Place Deadlock Markers in corners (without goals)
        for y,a in enumerate(self.sm):
            for x,b in enumerate(self.sm[y]):
                if x == 0 or x == (len(self.sm[0])-1) or \
                   y == 0 or (y == len(self.sm)-1):
                    continue
                if self.sm[y][x] == self.TILE_SPACE:
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

        xy_v = self.DirectView(self.sm)
        yx_v = self.Swap_XY_View(xy_v)
        for dead in self.getDeadlocks():
            (dx,dy) = dead
            connect_markers(dx, dy, xy_v)
            connect_markers(dy, dx, yx_v)

