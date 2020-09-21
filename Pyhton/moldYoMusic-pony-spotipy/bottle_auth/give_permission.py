from bottle import route, run, request, redirect
import spotipy
from spotipy import oauth2
from settings.settings import *

CACHE = '.spotipyoauthcache'

sp_oauth = oauth2.SpotifyOAuth( CRED_SPOTIPY_CLIENT_ID,
                                CRED_SPOTIPY_CLIENT_SECRET,
                                CRED_SPOTIPY_REDIRECT_URI,
                                scope=CRED_SCOPE,
                                cache_path=CACHE )

@route('/')
def index():
    access_token = ""
    token_info = sp_oauth.get_cached_token()

    if token_info:
        print("Found cached token!")
        access_token = token_info['access_token']
    else:
        url = request.url
        code = sp_oauth.parse_response_code(url)
        if code:
            print("Found Spotify auth code in Request URL! Trying to get valid access token...")
            token_info = sp_oauth.get_access_token(code)
            access_token = token_info['access_token']

    if access_token:
        print("Access token available! Trying to get user information...")
        sp = spotipy.Spotify(access_token)
        results = sp.current_user()
        return results

    else:
        # return htmlForLoginButton()
        auth_url = getSPOauthURI()
        redirect(auth_url)


@route('/callback/', method='GET')
def callback():
    url = request.url
    code = sp_oauth.parse_response_code(url)
    if code:
        print("Found Spotify auth code in Request URL! Trying to get valid access token...")
        token_info = sp_oauth.get_access_token(code)
        access_token = token_info['access_token']

        write_auth_token(access_token)
        return access_token

def getSPOauthURI():
    auth_url = sp_oauth.get_authorize_url()
    return auth_url

run(host='localhost', port=8888)
