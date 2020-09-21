import os

os.environ.setdefault('RELEASE_CHANNEL', 'prod')
RELEASE_CHANNEL = os.environ.get('RELEASE_CHANNEL')

if RELEASE_CHANNEL == 'dev':
    BASE_URL = 'http://127.0.0.1:5000'
else:
    BASE_URL = 'http://calhacks18.herokuapp.com'
