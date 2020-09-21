###  dagr.py
====
dagr.py (deviantArt gallery ripper) is a deviantArt image downloader script written in Python.  
It can download every image (deviation) in a gallery, as well as every favorited deviation a deviant may have.  
Originally developed at http://lovecastle.org/dagr/ (now defunct), development now continues on Github

Changelog is available [here](CHANGELOG.md)

### Dependencies
* a working Python installation (2.x or 3.x)
* robobrowser module (https://github.com/jmcarp/robobrowser), and its dependencies

### Gentoo installation
dagr is available in the voyageur overlay (https://cafarelli.fr/cgi-bin/cgit.cgi/voyageur-overlay/).
You can install it with layman:
```
# layman -a voyageur
# emerge dagr
```
### Current bugs and limitations
These should be tracked here: https://github.com/voyageur/dagr/issues

###  Usage
Here's an example of how to use the script:

```
$ python dagr.py
dagr.py v0.60 - deviantArt gallery ripper
Usage: dagr.py [-u username] [-p password] [-acfghoqrstv] [deviant]...
Example: dagr.py -u user -p 1234 -gsfv derp123 blah55
For extended help and other options, run dagr.py -h

$ python dagr.py -u username -p password -gs doo22
dagr.py v0.60 - deviantArt gallery ripper
Attempting to log in to deviantArt...
Logged in!
Current deviant: doo22
Ripping doo22's gallery...
```

### FAQ
- Will I be banned from deviantArt if I use dagr.py?  
Not likely. However, dagr.py could be blocked at any time. If you want to be sure your main account isn't banned, use a disposable account and a proxy.

- The deviantArt page says a deviant has 145 deviations but the program can only find 139!  
Sometimes deviantArt reports the wrong number of deviations in a gallery. This is because you can submit deviations exclusively to a group without having it show up in your gallery.

- Why can I not download mature deviations?  
You must use a deviantArt account that is able to view such deviations, and has "Show Deviations with Mature Content" enabled under Settings > Browsing.

### Older versions

These may still work or not, but have smaller dependencies:
* Versions before 0.50 only depend on Python, but can not download full images or other file types
* Versions 0.5x are python-2.x only, and depend on the mechanize module
