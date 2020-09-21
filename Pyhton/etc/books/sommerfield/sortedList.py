#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 08.10.2018
# Last Modified Date: 08.10.2018

_identity = lambda x : x

class SortedList(object):

    """Docstring for SortedList. """

    def __init__(self, sequence=None, key=None):
        """TODO: to be defined1.

        :sequece: TODO
        :key: TODO

        """
        self.__key = key or _identity
        assert hasattr(self.__key, "__call__")

        if sequence is None:            
            self.__list = []
        elif (isinstance(sequence, SortedList) and 
              sequence.key == self.__key):
            self.__list = sequence.__list[:]
        else:
            self.__list = sorted(list(sequence), key=self.__key)

    @property
    def key(self):
        """TODO: Docstring for key.
        :returns: TODO

        """
        return self.__key


    def add(self, value):
        """TODO: Docstring for add.
        :returns: TODO

        """
        index = self.__bisect_left(value)

        if index == len (self.__list):
            self.__list.append(value)
        else:
            self.__list.insert(index, value)
    
    def __bisect_left(self, value):
        key = self.__key(value)
        left, right = 0, len(self.__list)

        while left < right :
            middle = (left + right) // 2

            if self.__key(self.__list[middle]) < key:
                left = middle + 1

            else:
                right = middle


        return left

    def remove(self, value):
        index = self.__bisect_left(value)
        if 





        

