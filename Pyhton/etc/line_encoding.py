#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 13.10.2018
# Last Modified Date: 13.10.2018

def line_encode(dat):
    """TODO: Docstring for line_encode.

    :dat: TODO
    :returns: TODO

    """
    cmp = list()
    for _ in range(len(dat)):
        cmp.append(None)


    cidx = 0
    didx = 0
    
    
    try:
        while dat[didx] is not None:
            cmp_char = dat[didx]
            cnt = 1

            while cmp_char == dat[cnt+didx]:
                cnt += 1

            didx += cnt

            if cnt >= 4:
                cmp[cidx] = '@'
                cmp[cidx + 1] = cmp_char
                cmp[cidx + 2] = cnt

                cidx += 3
                # didx += cnt

            else:
                for _ in range(cnt):
                    cmp[cidx] = cmp_char
                    cidx += 1

                # didx += cnt
    except IndexError as i:
        cmp[cidx] = dat[didx]

    return cmp

def main():
    """TODO: Docstring for main.
    :returns: TODO

    """
    dat = "AABBBBCDEEEEEF"
    result_list = line_encode(dat)

    print( "".join(str(x) for x in result_list if x is not None))



if __name__ == "__main__":
    main()

