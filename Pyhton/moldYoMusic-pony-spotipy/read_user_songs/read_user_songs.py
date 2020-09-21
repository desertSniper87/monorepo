import sys
import spotipy
import spotipy.util as util

def read_user_songs(token: str) -> dict:
    """TODO: Docstring for read_user_songs.

    :token: str
    :returns: a dictionary of tracks
    :type: return: dict

    """

    # Create token here

    if token:
        number_of_songs_processed = 0
        sp = spotipy.Spotify(auth=token)
        results = sp.current_user_saved_tracks()

        items = results['items']

        number_of_songs_processed = len(items)
        print(f"{number_of_songs_processed} of songs processed")
        while results['next']:
            results = sp.next(results)
            items.extend(results['items'])

            number_of_songs_processed = len(items)
            print(f"{number_of_songs_processed} of songs processed")

        for item in items:
            track = item['track']
            print (track['name'] + ' - ' + track['artists'][0]['name'])

        return items
    else:
        # print("Can't get token for", username)
        scope = 'user-library-read'




