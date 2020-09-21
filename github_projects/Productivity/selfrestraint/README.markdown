SelfRestraint
=============

About
-----
SelfRestraint is a cross platform version of Steve Lambert's [SelfControl](http://github.com/slambert/selfcontrol), written in Python. It allows you to block distracting sites for a set amount of time, so you can use your computer and access the internet without having to worry about distracting sites.

Credits
-------
SelfRestraint was developed by [Parker Kuivila](http://parker.kuivi.la)
The UI and features were inspired by [Steve Lambert](http://visitsteve.com/)

License
-------
SelfRestraint is Free Software under the GPL. You are free to share, modify, and add to the code as you wish.

Installation
------------
It was not running on my PC.  There are no instruction on the original site. I did not bother running pyinstaller. And appearantly the provied binary is hardcoded. 

If you are running a Linux machine, the necessary dependencies (python, qt) are already in your system.

Just run
```python ~/SelfRestraint/SelfRestraint.py```.
To change the blacklist 
```vim .config/SelfRestraint/blocklist```

Todo
----
The ```inithost()``` function is not called properly. So, changing the Blockhost in the middle of the timer has no effect. 

