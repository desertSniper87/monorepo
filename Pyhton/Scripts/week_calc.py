#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 01.02.2018
# Last Modified Date: 01.02.2018
import time
import datetime
from datetime import date

start_date = datetime.date(2017, 4, 3)
today = date.today()
delta = today-start_date
print(delta.days)

unused_weeks = delta.days/7
unused_days = delta.days%7
print("Unused weeks",unused_weeks, "Unused days", unused_days)

used_weeks = 9
rm_wk = delta.days/7 - used_weeks
print("Remaining week ", rm_wk)

old_week = 21.5
print("New remaining time", old_week+rm_wk)
print("Today is: ", today)
