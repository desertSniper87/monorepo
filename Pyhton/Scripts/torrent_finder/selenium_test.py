from selenium import webdriver
import time

filename = 'deleted_torrents.txt'

lines = [line.rstrip('\n') for line in open(filename)]

for x in lines:
    browser = webdriver.Firefox()

    print("Opening data file... ", x)

    browser.get("https://torrentz2.eu/search?f="+x)
    # time.sleep(10)
    browser.close()

