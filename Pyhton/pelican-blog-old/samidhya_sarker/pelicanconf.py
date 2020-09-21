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
          ('Twitter', 'https://twitter.com/desertSniper87'),)

DEFAULT_PAGINATION = False

# Uncomment following line if you want document-relative URLs when developing
RELATIVE_URLS = True

# THEME = "/home/torsho/Dropbox/programming_code/Pyhton/pelican-blog/samidhya_sarker/pelican-themes/pelican-elegant-bs-4"
THEME = "/home/torsho/Dropbox/programming_code/Pyhton/pelican-blog/samidhya_sarker/pelican-themes/pelican-elegant"
# THEME = "/home/torsho/pelican-themes/pelican-bootstrap3"
# THEME = "/home/torsho/pelican-themes/abs"

PLUGIN_PATHS = ['/home/torsho/pelican-plugins']
# JINJA_ENVIRONMENT = {'extensions': ['jinja2.ext.i18n']}
# PLUGINS = ['i18n_subsites', 'pelican-cite']
# PLUGINS = ['pelican_javascript']

# I18N_SUBSITES = {
    # 'bd': {
        # 'SITENAME': 'তর্ষ@বাড়ি',
        # }
    # }

# PUBLICATIONS_SRC = 'content/pubs.bib'

STATIC_PATHS = ['haglSim', 'images']

ARTICLE_EXCLUDES = ['haglSim']

LANDING_PAGE_ABOUT = { 'title': "",
                        'details': """Hello I am Samidhya Sarker. I am an aspiring Software Engineer currently on the market.
                                      My old blogs can be found at 
                                      <a href="https://www.facebook.com/desertsniper87/notes"> Facebook Notes,</a>
                                      <a href="https://medium.com/@desertsniper87"> Medium, </a>
                                      <a href="https://desertsniper87.wordpress.com"> WordPress </a>
                                      """}
# ABOUT_ME = """Hello I am Samidhya Sarker. I am an aspiring Software Engineer currently on the market.
                                      # My old blogs can be found at 
                                      # <a href="https://www.facebook.com/desertsniper87/notes"> Facebook Notes,</a>
                                      # <a href="https://medium.com/@desertsniper87"> Medium, </a>
                                      # <a href="https://desertsniper87.wordpress.com"> WordPress </a>
                                      # <br/>
                                      # <h3>My Interests</h3>
                                      # <div class="container" style="width:500px;">
                                          # <div class="row"> 
                                               # <div class="col-sm-3">
                                                # <img class="img-responsive" src="images/gnu-card.jpg"/>
                                               # </div>
                                               # <div class="col-sm-3">
                                                # <img class="img-responsive" src="images/arch-card-demo.jpg"/>
                                               # </div>
                                          # </div>
                                          # <div class="row"> 
                                               # <div class="col-sm-3">
                                                # <img class="img-responsive" src="images/python-card.jpg"/>
                                               # </div>
                                               # <div class="col-sm-3">
                                                # <img class="img-responsive" src="images/vim-card-demo.jpg"/>
                                               # </div>
                                          # </div>
                                      # </div>
                                      # I play <a href="https://www.igdb.com/users/desertsniper87/lists/played"> Video Games </a> and watch <a href="https://www.imdb.com/user/ur44018698/ratings"> Movies </a>.
                                      # """
