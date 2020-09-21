# Imports the Google Cloud client library

from google.cloud import language
from google.cloud.language import enums
from google.cloud.language import types
import pickle
import pandas as pd
import numpy as np
# Instantiates a client
client = language.LanguageServiceClient()

# The text to analyze
text =  u'I am feeling super duper duper happy!! Lets party'
# u'I am feeling very very extremely sad.  I just want to cry'
text1 = u'I am feeling super duper duper happy!! Lets party'
document = types.Document(
    content=text,
    type=enums.Document.Type.PLAIN_TEXT)

# Detects the sentiment of the text
sentiment = client.analyze_sentiment(document=document).document_sentiment

print('Text: {}'.format(text))
print('Sentiment: {}, {}'.format(sentiment.score, sentiment.magnitude))

if (sentiment.score > 0):
    mood_score = sentiment.score  * 1.5
else:
    mood_score = sentiment.score  * 4

print(mood_score)

with open('lm.pickle', 'rb') as fi:
    lm = pickle.load(fi)

# new_labels = {"tempo": "bpm", "danceability": "dnce", "energy": "nrgy", "loudness": "dB", "liveliness": "live", "valence": "val", "duration_ms": "dur", "acousticness": "acous"}
data = pd.read_csv("test_set.csv")

def prep_data(frame):
    frame_data = frame
#     zero_bpm = frame_data[frame_data["bpm"] == 0].index[0]
#     frame_data = frame_data.drop([zero_bpm])
    frame_data["dur"] = frame_data["dur"].astype("float")
    return frame_data

def normalize(col):
    col_range = max(col) - min(col)
    avg = np.mean(col)
    return (col - avg)/col_range


def prep_features(tbl):
    tbl_norm = tbl
    tbl_norm["bpm"] = normalize(tbl_norm["bpm"])
    tbl_norm["nrgy"] = normalize(tbl_norm["nrgy"]*100)
    tbl_norm["dnce"] = normalize(tbl_norm["dnce"]*100)
    tbl_norm["val"] = normalize(tbl_norm["val"]* 100)
    tbl_norm["acous"] = normalize(tbl_norm["acous"] * 100)

    tbl_norm["dur"] = tbl_norm["dur"]/100
    return tbl_norm

#clean data and normalize features
data = prep_data(data)
data = prep_features(data)

def predict_songs(tbl):
    tbl_predicted = tbl
    predicted = lm.predict(tbl.loc[:, ["bpm", "nrgy", "dnce", "dB", "val", "dur", "acous"]])
    tbl_predicted["mood_predicted"] = predicted
    return tbl_predicted
predicted = predict_songs(data)

def find_predicted_songs(tbl, score, num_songs):
    songs = num_songs
    if songs > 25:
        songs = 25
    in_range = tbl

    in_range["dists"] = abs(in_range["mood_predicted"] - score)
    sort_by_dist = in_range.sort_values("dists")

    return sort_by_dist[:num_songs]
playlist = find_predicted_songs(predicted, mood_score, 25)
print(playlist)
