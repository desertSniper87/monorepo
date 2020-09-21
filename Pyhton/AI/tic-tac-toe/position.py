from copy import deepcopy

class Position:
    dim_x = 3
    dim_y = 3
    turns = ('x', 'o')

    """Contains the board"""

    def __init__(self, turn, board=None, move=None):
        """Create the board"""
        if board==None:
            self.board = [['-' for x in range(self.dim_x)] for y in range(self.dim_y)]
        else:
            self.board=board

        if move!=None:
            x = move[0]
            y = move[1]
            self.board[x][y] = turn
            # print("self.board: ", self.board)

            if turn=='o':
                self.turn='x'
            else:
                self.turn='o'
        else:
            self.turn = turn
        # print(self.board)

    def switch_turns(self):
        if self.turn is 'x':
            self.turn = 'o'
        else:
            self.turn = 'x'

    
    def __str__(self):
        return str(self.board)
    
    @classmethod
    def init_from_board(cls, turn, board):
        return cls(board=board, turn=turn)

    @classmethod
    def init_from_board_move(cls, turn, board, move):
        return cls(board=board, turn=turn, move=move)

    def move(self, i, j):
        """
        :i: int
        :j: int
        :returns: Position 
        """
        # print(self.turn)
        # print(self.board)
        newBoard = deepcopy(self.board)
        newBoard[i][j] = self.turn

        if self.turn=='x':
            return self.init_from_board('o', newBoard)
        else:
            return self.init_from_board('x', newBoard)


    def possible_moves(self):
        """
        :returns: list of tuples that contains the 
        x and y of possible moves

        """
        list = []
        for idx, i in enumerate(self.board):
            for jdx, j in enumerate(i):
                if j=='-':
                    list.append((idx, jdx))

        return list

    def win(self, player):
        """determines if board condition is win

        :turn: char: who will win
        :returns: boolean

        """
        if (\
               self.horizontal_win(player) or\
               self.vertical_win(player) or\
               self.diagonal_win(player) or\
               self.rev_diagonal_win(player)
           ):

            return True

        return False

    def horizontal_win(self, player):
        """Checks if line yields win

        :returns: Bool

        """
        for i in self.board:
            if self.check_line(i, player):
                return True

        return False

    def check_line(self, line, player):
            if len(set(line)) == 1 and player in set(line):
                return True


    def vertical_win(self, player):
        """Checks if line yields win

        :returns: Bool

        """
        for i in [list(k) for k in zip(*self.board)]:
            if self.check_line(i, player):
                return True
        return False
        # return True

    def diagonal_win(self, player):
        """Checks if line yields win

        :returns: Bool

        """
        for idx, i in enumerate(self.board):
            for jdy, j in enumerate(i):
                if idx==jdy and j!=player:
                    # print("idx, jdy, x, y: ", idx, jdy, i, j)
                    return False
        
        return True

    def rev_diagonal_win(self, player):
        """Checks if line yields win

        :returns: Bool

        """
        for idx, i in enumerate(self.board):
            for jdy, j in enumerate(i):
                if idx+jdy==2 and j!=player:
                    # print("idx, jdy, x, y: ", idx, jdy, i, j)
                    return False
        
        return True
        # return True

    def minimax(self):
        """Returns minimax score
        :returns: int

        """
        # print(self.possible_moves())
        if (self.win('x')):
            return 100
        if (self.win('o')):
            return -100
        if len(self.possible_moves())==0:
            return 0

        val = 0
        for i in self.possible_moves():
            if self.move(*i).win('x') and self.turn=='x':
                val += 50
                # print("*i: ", *i)
            else:
                for j in self.move(*i).possible_moves():
                    if self.move(*j).win('x') and self.turn=='x':
                        val += 10
                        # print("*j: ", *j)
                        self.showBoardMove((i, j), 'x')
                    if self.move(*j).win('o') and self.turn=='o':
                        val -= 10
                        # print("*j: ", *j)
                        self.showBoardMove((i, j), 'o')
            if self.move(*i).win('o') and self.turn=='o':
                val -= 50
                self.turn='x'

            else:
                # print("*i: ", *i)
                for j in self.move(*i).possible_moves():
                    if self.move(*j).win('x') and self.turn=='x':
                        val += 10
                        # print("*j: ", *j)
                        self.showBoardMove((i, j), 'x')
                    if self.move(*j).win('o') and self.turn=='o':
                        val -= 10
                        # print("*j: ", *j)
                        self.showBoardMove((i, j), 'o')

        return val
        
    def showBoardMove(self, move, turn):
        """Takes a move tuple and shows it the board

        :move: tuple
        :returns: None

        """
        # print(move)
        x0 = move[0][0]
        y0 = move[0][1]
        x1 = move[1][0]
        y1 = move[1][1]
        newBoard = deepcopy(self.board)
        newBoard[x0][y0] = turn
        newBoard[x1][y1] = turn

        # for i in newBoard:
            # print(i)

    def bestMove(self):
        best_move_score = None
        self_possible_moves = self.possible_moves()

        # print(self_possible_moves)
        for i in self.possible_moves():
            pos_copy = deepcopy(Position)
            # __import__('pudb').set_trace()
            print("self.board: ", self.board)
            # print("x.board: ", x.board)
            possible_move_score = pos_copy.init_from_board_move(turn=self.turn, board=self.board, move=i).minimax()
            x =pos_copy.init_from_board_move(turn=self.turn, board=self.board, move=i)
            self.switch_turns()
            # print(x.board)
            if best_move_score==None:
                best_move_score = possible_move_score
                best_move = i
            if (possible_move_score>best_move_score):
                best_move_score=possible_move_score
                best_move = i

        return best_move




    # @property
    # def board(self):
        # return self.__board
