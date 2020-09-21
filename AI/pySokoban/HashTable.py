import SokoMap


class HashTable:
    def __init__(self):
        self.table = {}

    def checkAdd(self, sokomap):
        key = str(sokomap.getBlocks() + [sokomap.getPlayer()])
        if key in self.table:
            return True
        else:
            self.table[key] = True
            return False

