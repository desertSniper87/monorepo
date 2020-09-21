#!/bin/bash
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 23.04.2019
# Last Modified Date: 23.04.2019

for i in {1..10};
do
    echo $i
    echo $i | telnet;
done
    
