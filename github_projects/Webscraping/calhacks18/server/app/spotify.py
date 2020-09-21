import base64
import os
import urllib.parse
import requests
import spotipy as spotipy

from flask import current_app, json

from app.base import BASE_URL

SPOTIFY_AUTH_BASE_URL = "https://accounts.spotify.com"
SPOTIFY_AUTH_URL = SPOTIFY_AUTH_BASE_URL + '/authorize'
SPOTIFY_TOKEN_URL = SPOTIFY_AUTH_BASE_URL + '/api/token'

CLIENT_ID = os.environ.get('SPOTIFY_CLIENT_ID')
CLIENT_SECRET = os.environ.get('SPOTIFY_CLIENT_SECRET')

CLIENT_BEARER = base64.b64encode(("{}:{}".format(CLIENT_ID, CLIENT_SECRET)).encode()).decode()

SCOPE = [
    'playlist-modify-public',
    'user-library-read',
]

CALLBACK_URL = BASE_URL + '/spotify/callback'
AUTH_URL = BASE_URL + '/spotify/auth'

def build_query_params(d):
    return '&'.join('{}={}'.format(k, urllib.parse.quote(v)) for k, v in d.items())


def auth_url():
    return '{}?{}'.format(SPOTIFY_AUTH_URL, build_query_params({
        'response_type': 'code',
        'redirect_uri': CALLBACK_URL,
        'scope': ' '.join(SCOPE),
        'client_id': CLIENT_ID
    }))


def logout_url():
    return 'https://spotify.com/logout'


def authorize(auth_token):
    headers = {
        'Authorization': 'Basic {}'.format(CLIENT_BEARER)
    }
    payload = {
        'grant_type': 'authorization_code',
        'code': auth_token,
        'redirect_uri': CALLBACK_URL
    }

    request = requests.post(SPOTIFY_TOKEN_URL, data=payload, headers=headers)

    response_data = json.loads(request.text)
    current_app.logger.info('response_data=' + str(response_data))
    if 'error' in response_data:
        return None
    return response_data['access_token']


def get_all_tracks(token, num_max_tracks=200):
    sp = spotipy.Spotify(auth=token)
    offset = 0
    limit = 20
    result = []
    curr = sp.current_user_saved_tracks(limit=min(limit, num_max_tracks - offset), offset=offset)
    while len([x['track'] for x in curr['items']]) > 0 and len(result) <= num_max_tracks:
        for item in curr['items']:
            result.append(item['track'])
        offset += limit
        curr = sp.current_user_saved_tracks(offset=offset)
    return result


def get_audio_features(token, tracks):
    sp = spotipy.Spotify(auth=token)
    tracks = [t["uri"] for t in tracks]
    tracks = [tracks[i * 50:(i + 1) * 50] for i in range(len(tracks) // 50 + 1)]
    result = []
    for item in tracks:
        result.extend(sp.audio_features(item))
    return result


def make_playlist(token, tracks, name):
    sp = spotipy.Spotify(auth=token)
    info = sp.current_user()
    current_app.logger.info(info)
    playlist = sp.user_playlist_create(info['id'], name)
    tracks = [t["uri"] for t in tracks]
    tracks = [tracks[i * 100:(i + 1) * 100] for i in range((len(tracks) + 99) // 100)]
    for item in tracks:
        sp.user_playlist_add_tracks(info['id'], playlist["id"], item)
    current_app.logger.info('did it')
