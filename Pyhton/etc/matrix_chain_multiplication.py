#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 09.11.2018
# Last Modified Date: 09.11.2018
def matrix_chain_multiplication(mat_dim_array):
    """TODO: Docstring for matrix_chain_multiplication.

    :mat_dim_array: array of dimensions, int
    :returns: num of min multiplications, int

    """
    l = len(mat_dim_array)
    
    m = [[0 for x in range(l)] for x in range(l)]

    for i in range(l-2):
        m[i][i+1] = mat_dim_array[i] * mat_dim_array[i+1] * mat_dim_array[i+2]

    for i in range (l):
        for j in range (2+i, l):
            m[i][j] = float('inf')
            for k in range (i, j):
                m[i][j] = min(m[i][j], (m[i][k] + m[k+1][j] + mat_dim_array[i]*mat_dim_array[k]*mat_dim_array[j]))

    __import__('pprint').pprint(m)




def main():
    array = [30, 35, 15, 5, 10, 20, 25]
    x = matrix_chain_multiplication(array)

    # print(x)

if __name__ == "__main__":
    main()
