#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 10.11.2018
# Last Modified Date: 10.11.2018

import sys
import spotipy
import spotipy.util as util

def get_user_info(userID):
    """TODO: Docstring for get_user_info.

    :userID: TODO
    :returns: TODO

    """
    pass

def main():
    username = 'desertsniper87'
    token = util.prompt_for_user_token(username)
    s = spotipy.Spotify(auth=token)
    pass

if __name__ == "__main__":
    main()
