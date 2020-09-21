import lyric_to_sentiment
import sys
import pandas as pd

def main():
 	inputs = sys.argv
 	start, end = inputs[0], inputs[1]
 	path = 'calhacksdata2clean.csv'
 	df = pd.read_csv(path)
 	df = df.drop_duplicates(subset='title', keep='first')
 	df = df[start:end]
 	lyric_to_sentiment.read_Data(df)


if __name__ == '__main__':
    main()