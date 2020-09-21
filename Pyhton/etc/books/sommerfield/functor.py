#!/usr/bin/env python3.6
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 08.10.2018
# Last Modified Date: 08.10.2018

class Strip(object):

    """Docstring for Strip. """

    def __init__(self, characters):
        """TODO: to be defined1.

        :characters: TODO

        """
        self.characters = characters

    def __call__(self, string):
        """TODO: Docstring for __call__.

        :string: TODO
        :returns: TODO

        """
        return string.strip(self.characters)
        

def main():
    strip_punctuation = Strip(",;:.!")
    print(strip_punctuation("Land ahoy!"))


if __name__ == "__main__":
    main()
