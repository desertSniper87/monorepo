#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 10.11.2018
# Last Modified Date: 10.11.2018

import re
def is_palindrome(string):
    """TODO: Docstring for is_palindrome.

    :string: TODO
    :returns: TODO

    """
    palindrome_regex = re.compile(r"^\w$")
    if palindrome_regex.match(string):
        return True
    return False


def main():
    while True:
        c = input()
        if is_palindrome(c):
            print("Palindrome")
        else:
            print("Not palindrome")

if __name__ == "__main__":
    main()
