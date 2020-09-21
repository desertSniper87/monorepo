import bcolors as bcolors
import openpyxl, os
from os.path import join
from itertools import chain
import re
import Levenshtein
import shutil
import eyed3

choice = '1'
MATCH_ARTIST_NAME = 1

print("Opening workbook...")
filename = 'playlist_2.xlsx'
wb = openpyxl.load_workbook(filename)

sheet = wb.get_active_sheet()
print(sheet)

# print("Please enter \n"
#       "1. Finding songs from music folders listed in playlist.xlsx\n"
#       "2. Find if songs already exists in MUSIC BACKUP and write x in B column\n")
# choice = input()

# if choice=='1':
#     search = chain.from_iterable(os.walk(path) for path in ('H:\\', 'O:\My Songs\Metal'))
# elif choice=='2':
#     search = os.walk('O:\Mobile Music Backup\\')

print("Reading rows")

for row in range(2, sheet.max_row + 1):
    if (sheet['B' + str(row)].value == None and
        sheet['A' + str(row)].font.i == False and
        sheet['A' + str(row)].fill.start_color.index == 'FFF2F2F2' or '00000000'):
        lookfor = sheet['A' + str(row)].value
        # print(lookfor)
        # for root, dirs, files in chain.from_iterable(os.walk(path)for path in search_dir):
        # for root, dirs, files in os.walk('O:\Mobile Music Backup\\'):
        for root, dirs, files in chain.from_iterable(os.walk(path)\
                                 for path in ('H:\\', 'O:\My Songs\Metal')):
        # for root, dirs, files in os.walk('O:\Mobile Music Backup\\'):
            for name in files:
                file = name

                if name.endswith('.mp3'):
                    name = name[:-4]

                name = re.sub(r'\d+', '', name)
                name = re.sub(r'[ .-]', '', name)
                name.lstrip().rstrip()

                lookfor = re.sub(r'[ ]', '', lookfor)
                lookfor.rstrip().lstrip()

                if name == '':
                   break


                # print(name, "        ===============          ", lookfor.rsplit(' ', 1)[0])

                if (Levenshtein.distance(name, lookfor) < 10):
                    if (name in lookfor) or (lookfor in name):
                        # print(bcolors.BLUE + "Match found" + bcolors.ENDC)
                        # print(name, "        ===============          ", lookfor.rsplit(' ', 1)[0])
                        # print(os.path.join(root, file))
                        # print("Artist Name: ", )
                        artist_name = None
                        try :
                            artist_name =\
                            eyed3.load(os.path.join(root, file)).tag.artist
                            # print (artist_name)
                        except AttributeError:
                            pass

                        # print(artist_name, "             ================                ", sheet['D' + str(row)].value )
                        # if artist_name == sheet['D' + str(row)].value.rstrip().lstrip():
                            # print(name, "        ===============          ", lookfor.rsplit(' ', 1)[0])
                            # print(artist_name, "             ================                ", sheet['D' + str(row)].value )
                            # print(os.path.join(root, file))


                        # print('Enter y to change value of B row')
                        # i = input()
                        # if i=='y':

                        ############################
                        ###
                        if choice == '2':
                            print("Changing value of B row")
                            sheet['B' + str(row)].value = 'x'
                        ###
                        ############################

                        if MATCH_ARTIST_NAME == 1:
                             if artist_name != sheet['D' + str(row)].value.rstrip().lstrip():
                                 break



                        if choice == '1':
                            if not os.path.exists(os.path.join('c:/music', sheet['D'+str(row)].value)):
                                os.makedirs(os.path.join('c:/music', sheet['D'+str(row)].value))
                            # if os.path.exists(join('c:/music'+sheet['B' + str(row)].value, file)):
                            if os.path.exists(join('c:/music', file)):
                                try:
                                    os.makedirs(os.path.join('c:/music', name))
                                    # os.makedirs(os.path.join('c:/music'+sheet['D' + str(row)].value, name))
                                    shutil.copy2(os.path.join(root, file), os.path.join('c:/music', name))
                                except FileExistsError:
                                    print( bcolors.FAIL, "File", name, "Already Exists in subdir", bcolors.ENDC)
                            else:
                                try:
                                    shutil.copy2(os.path.join(root, file), 'c:/music')
                                    # shutil.copy2(os.path.join(root, file), 'c:/music'+sheet['B' + str(row)].value)
                                except FileExistsError:
                                    print(bcolors.WARN, "File", name, "Already Exists in root", bcolors.ENDC)

if choice == '2':
    wb.save(filename)

