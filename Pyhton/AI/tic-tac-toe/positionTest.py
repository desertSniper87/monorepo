import unittest

from position import *

class Positiontest(unittest.TestCase):
    def test_testNew(self):
        position = Position(turn='x')
        # self.assertEqual(position.turn, 'x')
        self.assertEqual(position.board, [['-', '-', '-'], ['-', '-', '-'], ['-', '-', '-']])

    
    def test_move(self):
        position = Position(turn='x')
        p = position.move(0, 0)

        expected = [['x', '-', '-'], ['-', '-', '-'], ['-', '-', '-']]
        self.assertEqual(p.board, expected)

        self.assertEqual(p.turn, 'o')

    def test_possible_moves(self):
        expected = [['x', 'o', 'x'], ['-', '-', '-'], ['-', '-', '-']]
        position = Position('x').move(0, 0).move(0, 1).move(0, 2)
        self.assertEqual(position.board, expected)
        expected = [(1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)]
        self.assertEqual(position.possible_moves(), expected)

        position = Position.init_from_board(turn='x', board=[['x', 'o', 'x'],\
                                                             ['x', 'x', 'o'],\
                                                             ['o', 'x', 'o']])
        expected = []
        self.assertEqual(position.possible_moves(), expected)

    def test_move(self):
        position = Position(turn='x')
        position = Position('x').move(0, 0).move(0, 1).move(1, 1).move(1, 2).move(2, 2)
        self.assertEqual(position.board, [['x', 'o', '-'], ['-', 'x', 'o'], ['-', '-', 'x']])

        self.assertTrue(position.win(player='x'))

    def test_win(self):
        """

        if position win == true, player wins

        """
        position = Position.init_from_board(turn='x', board=[['x', 'o', '-'], ['-', 'x', 'o'], ['-', '-', 'x']])
        self.assertTrue(position.win(player='x'))

        position = Position.init_from_board(turn='x', board=[['x', 'x', 'x'], ['-', '-', 'o'], ['-', '-', '-']])
        self.assertTrue(position.win(player='x'))

        position = Position.init_from_board(turn='x', board=[['o', 'o', 'o'], ['-', '-', 'x'], ['-', '-', '-']])
        self.assertTrue(position.win(player='o'))

        position = Position.init_from_board(turn='x', board=[['x', '-', '-'], ['x', '-', 'o'], ['x', 'o', '-']])
        self.assertTrue(position.win(player='x'))

        position = Position.init_from_board(turn='x', board=[['x', '-', '-'], ['x', 'x', 'o'], ['o', 'o', 'x']])
        self.assertTrue(position.win(player='x'))

        position = Position.init_from_board(turn='x', board=[['o', '-', '-'], ['x', 'o', 'x'], ['x', 'o', 'o']])
        self.assertTrue(position.win(player='o'))

        position = Position.init_from_board(turn='x', board=[['-', '-', 'x'], ['-', 'x', 'o'], ['x', 'o', 'x']])
        self.assertTrue(position.win(player='x'))

    def test_best_move(self):
        position = Position.init_from_board(turn='x', board=[['x', 'x', 'o'],
                                                             ['-', 'x', 'o'],
                                                             ['x', 'o', '-']])
        self.assertEqual((2, 2), position.bestMove())

        position = Position.init_from_board(turn='o', board=[['x', '-', '-'],
                                                             ['-', '-', '-'],
                                                             ['-', '-', '-']])
        self.assertEqual((0, 1), position.bestMove())

    def test_minimax(self):
        position = Position.init_from_board(turn='x', board=[['x', 'x', 'x'],
                                                             ['-', 'x', 'o'],
                                                             ['x', 'o', 'x']])
        self.assertEqual(100, position.minimax())

        position = Position.init_from_board(turn='o', board=[['o', 'o', 'o'],
                                                             ['-', 'x', 'x'],
                                                             ['o', '-', '-']])
        self.assertEqual(-100, position.minimax())
 
        position = Position.init_from_board(turn='o', board=[['x', 'o', 'x'],
                                                             ['-', '-', '-'],
                                                             ['-', '-', '-']])
        self.assertEqual(0, position.minimax())

        position = Position.init_from_board(turn='o', board=[['x', '-', 'x'],
                                                             ['o', '-', '-'],
                                                             ['-', '-', '-']])
        self.assertEqual(0, position.minimax())

        position = Position.init_from_board(turn='o', board=[['x', '-', 'x'],
                                                             ['-', '-', '-'],
                                                             ['-', 'o', '-']])
        self.assertEqual(0, position.minimax())

        position = Position.init_from_board(turn='x', board=[['x', 'x', ' '],\
                                                             ['-', 'o', '-'],\
                                                             ['-', '-', '-']])
        self.assertEqual(99, position.minimax())

        position = Position.init_from_board(turn='o', board=[['o', 'o', ' '],\
                                                             ['-', 'x', '-'],\
                                                             ['-', '-', '-']])
        self.assertEqual(-99, position.minimax())
        


        
def main():
    try:
        unittest.main()
    except Exception:
        print(Exception)

if __name__ == "__main__":
    main()
