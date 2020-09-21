from flask import Flask


def create_app():
    fapp = Flask(__name__)

    from . import app
    fapp.register_blueprint(app.bp)

    return fapp
