#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 29.12.2018
# Last Modified Date: 29.12.2018
from typing import TypeVar
Board = TypeVar('Board')

from settings import CONST_DIM

from copy import deepcopy as _cpy
from pprint import pprint



def make_goal_matrix():
    goal = [[(x + y * CONST_DIM) % CONST_DIM**2 for x in range(1, CONST_DIM + 1)] for y in range(CONST_DIM)]
    return goal


class Solver(object):
    """Docstring for Solver. """

    # Board initial

    def __init__(self):
        """TODO: to be defined1. """

class Board(object):

    """Docstring for Board. """


    def __init__(self, blocks=None, ):
        """TODO: to be defined. """

        self.blocks = blocks
        self.f = 0
        self.g = 0
        self.h = self.manhattan()



    def print_elements(self, board_name: str) -> None:
        """TODO: Docstring for print_elements.
        :returns: None

        """
        print(board_name)
        pprint(self.blocks, width=15)

    def is_goal(self):
        """TODO: Docstring for is_goal.
        :returns: bool

        """
        goal = make_goal_matrix()
        n = 1
        for i in range(CONST_DIM):
            for j in range(CONST_DIM):
                if self.blocks[i][j] != goal[i][j]:
                    return False


        return True


    def hamming(self):
        """TODO: Docstring for hamming.

        :returns: TODO

        """
        goal = make_goal_matrix()
        c = 0
        hamming = 0

        for i in range(CONST_DIM):
            for j in range(CONST_DIM):
                goal[i][j] = c

                if c == max():
                    c = 0

                else:
                    c+=1

                if self.blocks[i][j] != 0 and self.blocks[i][j] != goal[i][j]:
                    hamming += 1


        return hamming

    def manhattan(self):
        """TODO: Docstring for manhattan.
        :returns: TODO

        """
        goal = make_goal_matrix()


        row_difference = 0
        column_difference = 0

        for idx, i in enumerate(self.blocks):
            for jdx, j in enumerate(i):
                if j == 0:
                    continue
                # print("goal[i][j]: ", goal[idx][jdx])
                # print('idx, jdx, i, j', idx, jdx, i, j)

                for sx, sublist in enumerate(goal):
                    if j in sublist:
                        row_difference += abs(idx - sx)

                for sx, sublist in enumerate([*zip(*goal)]):
                    if j in sublist:
                        column_difference += abs(jdx - sx)


        return row_difference + column_difference

    def column_containing_blank_piece(self) -> int:
        """ Indexed by zero
        :returns: TODO

        """
        transpose_matrix = [*zip(*self.blocks)]

        for idx, i in enumerate(transpose_matrix):
            if 0 in i:
                return idx

        print("blank not in board")


        # return i


    def row_containing_blank_piece(self) -> int:
        """ Indexed by zero
         :returns: TODO

         """
        for idx, i in enumerate(self.blocks):
            if 0 in i:
                return idx

        print("blank not in board")


    def size(self):
        """TODO: Docstring for size.

        :returns: TODO

        """
        return CONST_DIM**2


    def inversion_count(self):
        size = 0
        demo = 0
        len = CONST_DIM ** 2
        array = [0 for _ in len]

        for i in range(CONST_DIM):
            for j in range(CONST_DIM):
                if self.blocks[i][j] > 0:
                    array[demo] = self.blocks[i][j]
                    demo += 1

        size = invC(array)
        return size

    def invC(self, array):
        count = 0
        for i in range(len(array)):
            for j in range(i):
                if array[i] < array[j]:
                    count += 1

        return count

    def is_move_possible(self, direction: str) -> bool:
        current_zero_row = self.row_containing_blank_piece()
        current_zero_column = self.column_containing_blank_piece()

        board_move_limit = CONST_DIM - 1

        if ((direction == 'UP' and current_zero_row == 0) or
                (direction == 'DOWN' and current_zero_row == board_move_limit) or
                (direction == 'LEFT' and current_zero_column == 0) or
                (direction == 'RIGHT' and current_zero_column == board_move_limit)):
                    return False

        return True


    def go(self, direction: str) -> Board:
        current_zero_row = self.row_containing_blank_piece()
        current_zero_column = self.column_containing_blank_piece()

        if self.is_move_possible(direction) is False:
            raise ValueError(f'Moving {direction} Not possible')

        blocks = _cpy(self.blocks)

        if direction == 'UP':
            x_position = current_zero_row - 1
            y_position = current_zero_column

        elif direction == 'DOWN':
            x_position = current_zero_row + 1
            y_position = current_zero_column

        elif direction == 'LEFT':
            x_position = current_zero_row
            y_position = current_zero_column - 1

        elif direction == 'RIGHT':
            x_position = current_zero_row
            y_position = current_zero_column + 1

        swapping_piece = blocks[x_position][y_position]

        blocks[x_position][y_position] = 0
        blocks[current_zero_row][current_zero_column] = swapping_piece

        return Board(blocks)





