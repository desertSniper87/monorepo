#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 08.10.2018
# Last Modified Date: 08.10.2018
def binary_search(array, target):
    """TODO: Docstring for binary_search.

    :array: TODO
    :target: TODO
    :returns: TODO

    """
    array = sorted(array)
    
    left = 0
    right = len(array) - 1

    while left < right:
        __import__('pudb').set_trace()
        mid = (left + right) //  2

        if array[mid] < target :
            left = mid + 1

        elif array[mid] > target :
            right = mid - 1

        else:
            return mid

    return None


print(binary_search([2, 1, 3, 4, 7, 6, 9], 3))


