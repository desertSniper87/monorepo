#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 08.10.2018
# Last Modified Date: 08.10.2018

def generator_yield(d):
    """TODO: Docstring for generator_yield.

    :d: TODO
    :returns: TODO

    """
    for key in sorted(d):
        yield key, d[key]

def generator_list_comprehension(d):
    return ((key, d[key]) for key in sorted(d))


def main():
    d = {
         'a': ord('a'), 
         'z': ord('z'),
         'k': ord('k')
        }

    for i in generator_yield(d):
        print(i)

    for i in generator_list_comprehension(d):
        print(i)


if __name__ == "__main__":
    main()

