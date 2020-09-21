import pandas as pd
import numpy as np
import sentiment
import getlyric

#Function call to getlyrics

def get_Sentiment(lyrics):
	# print(lyrics[:10])
	return sentiment.sample_analyze_sentiment(lyrics)

def get_Lyrics(title, artist):
	getlyric.getLy(title, artist)
	with open('lyric_view.txt', 'r') as myfile:
		data = myfile.read()
	return data

def read_Data(df):
	# df = pd.read_csv(path)
	# df = df.drop_duplicates(subset='title', keep='first')
	titles = df['title'].values.tolist()
	artists = df['artist'].values.tolist()
	mag = []
	sent = []
	for i in range(len(titles)):
		lyr = get_Lyrics(titles[i], artists[i])
		senti, magn = get_Sentiment(lyr)
		mag.append(magn)
		sent.append(senti)
	df['Magnitude'] = mag
	df['Sentiment'] = sent
	with_Mag_Sent = pd.ExcelWriter('data' + str(len(df)) + '.xlsx')
	df.to_excel(with_Mag_Sent,'Sheet1')
	with_Mag_Sent.save()

print(read_Data('calhacksdata2clean.csv'))

