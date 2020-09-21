from logging.config import dictConfig
from flask import Blueprint, json, Response, redirect, request, current_app

from app import spotify, classifier_util
from app.base import RELEASE_CHANNEL

dictConfig({
    'version': 1,
    'formatters': {'default': {
        'format': '[%(asctime)s] %(levelname)s in %(module)s: %(message)s',
    }},
    'handlers': {'wsgi': {
        'class': 'logging.StreamHandler',
        'stream': 'ext://flask.logging.wsgi_errors_stream',
        'formatter': 'default'
    }},
    'root': {
        'level': 'DEBUG' if RELEASE_CHANNEL == 'dev' else 'INFO',
        'handlers': ['wsgi']
    }
})

bp = Blueprint('app', __name__)


def get_access_token():
    return request.headers.get('Authorization', '')


@bp.route('/')
def index():
    return 'Hey this is working!'


def get_all_tracks_with_features(access_token):
    all_tracks = spotify.get_all_tracks(access_token, 100)
    all_tracks_features = spotify.get_audio_features(access_token, all_tracks)
    [track.update(features) for track, features in zip(all_tracks, all_tracks_features)]
    return all_tracks


def dumps_playlist(playlist):
    return json.dumps([{
        'id': track['id'],
        'name': track['name'],
    } for track in playlist])


@bp.route('/playlist/create/mood')
def playlist_create_mood():
    access_token = get_access_token()
    mood = float(request.args.get('mood'))
    rock = bool(request.args.get('rock', ''))

    if not access_token:
        return '{"error":"Not authorized"}', 403

    # Getting all tracks
    current_app.logger.info('Getting all tracks & features')
    all_tracks = get_all_tracks_with_features(access_token)

    # Classifying things
    current_app.logger.info('Classifying things')
    playlist = classifier_util.suggest_playlist_from_mood(all_tracks, mood)

    if rock:
        current_app.logger.info('Creating playlist')
        playlist_name = 'mood %s' % mood
        spotify.make_playlist(access_token, playlist, playlist_name)
        return 'Yay, the playlist "%s" is created!' % playlist_name
    else:
        current_app.logger.info('Outputting playlist')
        return dumps_playlist(playlist)


@bp.route('/playlist/create/text')
def playlist_create_text():
    access_token = get_access_token()
    text = request.args.get('text').strip()
    rock = bool(request.args.get('rock', ''))

    if not access_token:
        return '{"error":"Not authorized"}', 403

    # Getting all tracks
    current_app.logger.info('Getting all tracks & features')
    all_tracks = get_all_tracks_with_features(access_token)

    # Classifying things
    current_app.logger.info('Classifying things')
    playlist = classifier_util.suggest_playlist_from_text(all_tracks, text)

    if rock:
        current_app.logger.info('Creating playlist')
        playlist_name = 'feel %s' % text[:10]
        spotify.make_playlist(access_token, playlist, playlist_name)
        return 'Yay, the playlist "%s" is created!' % playlist_name
    else:
        current_app.logger.info('Outputting playlist')
        return dumps_playlist(playlist)


@bp.route('/spotify/auth')
def spotify_auth():
    return redirect(spotify.auth_url())


@bp.route('/spotify/logout')
def spotify_logout():
    return redirect(spotify.logout_url())


@bp.route('/spotify/callback')
def spotify_callback():
    access_token = spotify.authorize(request.args.get('code')) or ''

    if access_token:
        # hash = hashlib.sha1()
        # hash.update(str(time.time()).encode('utf-8'))
        # hash.hexdigest()
        # user_id = hash.hexdigest()
        user_id = access_token
    else:
        user_id = ''

    return '''<script type="text/javascript">
// Thanks to: https://www.phodal.com/blog/react-native-onmessage-couldnt-read-postmessage-issue/

function awaitPostMessage() {
  var isReactNativePostMessageReady = !!window.originalPostMessage;
  var queue = [];
  var currentPostMessageFn = function store(message) {
    if (queue.length > 100) queue.shift();
    queue.push(message);
  };
  if (!isReactNativePostMessageReady) {
    var originalPostMessage = window.postMessage;
    Object.defineProperty(window, 'postMessage', {
      configurable: true,
      enumerable: true,
      get: function () {
        return currentPostMessageFn;
      },
      set: function (fn) {
        currentPostMessageFn = fn;
        isReactNativePostMessageReady = true;
        setTimeout(sendQueue, 0);
      }
    });
    window.postMessage.toString = function () {
      return String(Object.hasOwnProperty).replace('hasOwnProperty', 'postMessage');
    };
  }

  function sendQueue() {
    while (queue.length > 0) window.postMessage(queue.shift());
  }
}

window.onload = function () {
    awaitPostMessage();

    const USER_ID = '%s';
    document.write('Here is your user id: ' + USER_ID + '<br>');
    window.postMessage(USER_ID);
    document.write('The page should disappear soon :o');
};
</script>''' % user_id


@bp.route('/mock/playlists/<string:mood>')
def mock_playlist(mood):
    """
    Some mock data available at
        /mock/playlists/happy
        /mock/playlists/sad
    """

    mock_playlists = {
        'happy': [
            'Happy song 1',
            'Happy song 2',
        ],
        'sad': [
            'Sad song 1',
            'Sad song 2',
        ],
    }

    return Response(json.dumps(mock_playlists[mood] if mood in mock_playlists else []), 'application/json')


@bp.route('/mock/auth/echo-authorization')
def mock_auth_echo_authorization():
    return request.headers.get('Authorization', '')
