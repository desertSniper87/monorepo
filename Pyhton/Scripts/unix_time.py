#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Last Modified Date: 01.02.2018
import time
import datetime
from datetime import timedelta

#This is the day of the time. Meaning at s, 04:00 the time has begun
s = "30/11/2017"       #After midnight
ep = time.mktime(datetime.datetime.strptime(s, "%d/%m/%Y").timetuple())
print (ep)

n = 5
ep_time = datetime.datetime.fromtimestamp(ep)
n_ago_s = ep_time
# print (time.strftime('%d/%m/%Y', time.localtime(n_ago_s)))
while (n_ago_s<datetime.datetime.now()):
    n_ago_s += timedelta(days=n)
# if n_ago_s>datetime.datetime.now():
print("The last day was", (n_ago_s-timedelta(days=n)).strftime("%A %d %B %Y"), "After midnight")
print("The next one is,", n_ago_s.strftime("%A %d %B %Y"))
print ("04:00 am")
