from typing import List

from solver import Board, CONST_DIM

import random

from copy import deepcopy as _cpy

def random_initial_blocks() -> List[List[int]]:
    row_sample = []
    initial_blocks = []

    for i in random.sample(range(CONST_DIM ** 2), CONST_DIM ** 2):
        row_sample.append(i)
        if len(row_sample) == CONST_DIM:
            initial_blocks.append(row_sample)
            row_sample = []

    return initial_blocks


if __name__ == '__main__':

    # initial_blocks = random_initial_blocks()
    # initial_blocks = [
        # [8,  1,  3],
        # [4,  0,  2],
        # [7,  6,  5]
    # ]
    initial_blocks = [
        [0,  1,  3],
        [4,  2,  5],
        [7,  8,  6]
    ]

    DIRECTIONS = ['UP', 'DOWN', 'LEFT', 'RIGHT']

    initial = Board(initial_blocks)
    initial.print_elements("Initial")

    current_move = initial

    while not current_move.is_goal():
        possible_moves: List[Board] = []
        for direction in DIRECTIONS:
            if current_move.is_move_possible(direction):
                new = current_move.go(direction)
                possible_moves.append(new)



            else:
                print(f'Moving in {direction} not possible.')

        current_move = min(possible_moves, key= lambda x: x.manhattan())
        current_move.print_elements('recomended')
        print('recomended_move.manhattan()', current_move.manhattan())
