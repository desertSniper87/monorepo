#!/usr/bin/env python

import re
from setuptools import setup

def find_version(fname):
    """Attempts to find the version number in the file names fname.
    Raises RuntimeError if not found.

    """
    version = ''
    with open(fname, 'r') as fp:
        reg = re.compile(r'__version__ *= *[\'"]([^\'"]*)[\'"]')
        for line in fp:
            m = reg.match(line)
            if m:
                version = m.group(1)
                break
    if not version:
        raise RuntimeError('Cannot find version information')
    return version

__version__ = find_version('dagr/dagr.py')

setup(
    name='dagr',
    version=__version__,
    description='a deviantArt image downloader script written in Python',
    author='Bernard Cafarelli',
    url='https://github.com/voyageur/dagr',
    scripts=['dagr/dagr.py'],
    packages=(),
    install_requires=["robobrowser == 0.5.1"],
)
