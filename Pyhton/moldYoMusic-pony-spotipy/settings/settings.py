CRED_SPOTIFY_USERNAME = 'desertsniper87'
CRED_SPOTIFY_TOKEN = 'e67ca44247e84f2f929ec1192cb4c4c4'

CRED_SCOPE = 'user-library-read, playlist-read-private'

CRED_SPOTIPY_CLIENT_ID= '60d0301e44af4e7eb637c5dcab3e42db'
CRED_SPOTIPY_CLIENT_SECRET= 'e67ca44247e84f2f929ec1192cb4c4c4'
CRED_SPOTIPY_REDIRECT_URI= 'http://localhost:8888/callback/'

# CRED_TOKEN = 'BQC1IifQQstKKKxTzNJw6YmP6bDuYUFOX3nZYXleLjd8f_ydjEHA1Juewdup0tvdFrB7GfredgwQ8ZpBcBd-ZqaOIY9aLAlZ9OP6PY9iRdmCqF5n1z19UZ4aDYP_rcGaTUSicYGeGFdW4VLhtFBPbrOtPTlj3TEJhJ7L3NfP'


def write_auth_token(token):
    with open("../auth.txt", "w+") as file:
        file.write(token)
        file.close()


def read_auth_token():
    with open("auth.txt", "r") as file:
        return file.read()
        file.close()