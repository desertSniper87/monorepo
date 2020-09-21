import fileinput
import re
import random

publishers = ['Realbuzz', 'Zoombeat', 'Oba', 'Mymm', 'Voonder', 'Flipstorm', 'Cogidoo', 'Kanoodle', 'Livefish', 'Babbleset', 'Teklist', 'Edgewire', 'Skilith', 'Oba', 'Agivu', 'Riffpath', 'Digitube', 'DabZ', 'Janyx', 'Feedbug']

with fileinput.FileInput('books.sql') as file:
    for line in file:
        text_to_replace = re.search('[9][9][8-9][0-9]', line)
        # if text_to_replace.group(0)!=None
        # print (text_to_replace.group(0))
        # print(line.replace(text_to_replace.string, random.choice(publishers)), end = '')
        print(line.replace(text_to_replace.group(0), random.choice(publishers)), end = '')
