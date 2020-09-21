from pony.orm import *

sql_debug(True)
db = Database("sqlite", "mum.sqlite", create_db=True)

class Song(db.Entity):
    song_id = PrimaryKey(int, auto=True)
    spotify_id = Required(str)
    artist_name = Required(str)
    name = Required(str)
    album_name = Required(str)

db.generate_mapping(create_tables=True)

@db_session
def create_db_from_all_spotify_songs(items):
    """TODO: Docstring for create_db.

    :songs: dict
    :returns: TODO

    """
    songs = []
    for item in items:
        song = Song(spotify_id=item['track']['id'],
                    artist_name=item['track']['artists'][0]['name'],
                    name=item['track']['name'],
                    album_name=item['track']['album']['name'])
        songs.append(song)

    commit()

    print(items)



    
