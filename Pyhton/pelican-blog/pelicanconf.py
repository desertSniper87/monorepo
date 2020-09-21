#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = 'Samidhya Sarker'
SITENAME = """
<span style=color:black;">[</span><span style="color:darkblue;">torsho</span><span style="color:#AA1032;">@</span><span style=color:black;">localhost]</span>
"""
SITEURL = 'https://desertSniper87.github.io'

PATH = 'content'

TIMEZONE = 'Asia/Dhaka'

DEFAULT_LANG = 'en'

DIRECT_TEMPLATES = (('index', 'tags', 'categories', 'archives', 'search'))

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None

# Blogroll
LINKS = (('Pelican', 'http://getpelican.com/'),
         ('Python.org', 'http://python.org/'),
         ('Jinja2', 'http://jinja.pocoo.org/'),
         ('You can modify those links in your config file', '#'),)

# Social widget
SOCIAL = (('Facebook', 'http://www.facebook.com/desertsniper87'),
          ('Twitter', 'https://twitter.com/desertSniper87'),
          ('GitHub', 'https://github.com/desertSniper87'),
          ('LinkedIn', 'https://www.linkedin.com/in/desertsniper87/'),
          ('stack-overflow', 'https://stackoverflow.com/users/7154462/desertsniper87'),
          ('reddit', 'https://www.reddit.com/user/desertSniper87/'),
          )

DEFAULT_PAGINATION = False

# Uncomment following line if you want document-relative URLs when developing
RELATIVE_URLS = True

THEME = "./themes/elegant"

PLUGIN_PATHS = ['./plugins/']
PLUGINS = ['sitemap']

STATIC_PATHS = ['haglSim', 'images', 'extra']

EXTRA_PATH_METADATA = {
    # 'extra/custom.css': {'path': 'custom.css'},
    # 'extra/robots.txt': {'path': 'robots.txt'},
    'extra/favicon.ico': {'path': 'favicon.ico'},  # and this
    # 'extra/CNAME': {'path': 'CNAME'},
    # 'extra/LICENSE': {'path': 'LICENSE'},
    # 'extra/README': {'path': 'README'},
}

COMMENTO_SITENAME = 'desertsniper87.github.io'

ARTICLE_EXCLUDES = ['haglSim']

LANDING_PAGE_ABOUT = { 'title': "",
                        'details': """Hello I am Samidhya Sarker. I am an aspiring Software Engineer currently working at Concitus.
                                      My old blogs can be found at 
                                      <a href="https://www.facebook.com/desertsniper87/notes"> Facebook Notes,</a>
                                      <a href="https://medium.com/@desertsniper87"> Medium, </a>
                                      <a href="https://desertsniper87.wordpress.com"> WordPress </a>
                                      """}


SITEMAP = {
    'exclude': ['tag/', 'category/', 'archives']
}