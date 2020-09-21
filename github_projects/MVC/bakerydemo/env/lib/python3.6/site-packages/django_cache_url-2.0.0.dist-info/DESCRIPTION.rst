
django-cache-url
~~~~~~~~~~~~~~~~

This simple Django utility allows you to utilize the
`12factor <http://www.12factor.net/backing-services>`_ inspired
``CACHE_URL`` environment variable to configure your Django application.


Usage
-----

Configure your cache in ``settings.py``::

    CACHES={'default': django_cache_url.config()}

Nice and simple.


