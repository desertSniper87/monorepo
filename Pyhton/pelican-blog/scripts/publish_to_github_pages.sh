#!/usr/bin/env bash

./env/bin/pelican content -o output -s pelicanconf.py
./env/bin/ghp-import output
git push git@github.com:desertsniper87/desertsniper87.github.io.git gh-pages:master --force

