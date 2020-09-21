import webbrowser

from read_user_songs.read_user_songs import *
from settings.settings import *
from database.database import *


'''Getting Auth Credentials'''
webbrowser.open("http://localhost:8888/")
token = read_auth_token()


'''Getting a dictionary full of user songs.'''
items = read_user_songs(token)

'''Creating table and populating'''
create_db_from_all_spotify_songs(items)

