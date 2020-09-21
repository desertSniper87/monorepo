import sqlite3
from sqlite3 import Error

from selenium import webdriver
import time
 
def create_connection(db_file):
    """ create a database connection to a SQLite database """
    try:
        conn = sqlite3.connect(db_file) #??????????
        print(sqlite3.version)
        return conn
    except Error as e:
        print(e)
    # finally:
        # conn.close()

 
if __name__ == '__main__':
    conn = create_connection("playlist.sqlite3")
    cur = conn.cursor()

    cur.execute("SELECT COUNT(*) FROM playlist_youtube ")
    n = cur.fetchone()[0]
    i = 906
    while(i<n):
        print(i)
        cur.execute("""SELECT youtube_link FROM playlist_youtube 
                        WHERE song_id=?""", (i, ))

        if cur.fetchone()[0] is not None:
            print("Playlist link already exists for Song")
            i+=1
            continue
        
        cur.execute("""SELECT song_name FROM playlist WHERE song_id=?""", (i, ))
        # print(cur.fetchone())

        # fp = webdriver.FirefoxProfile('C:\\Users\Torsho\AppData\Local\Mozilla\Firefox\Profiles\mj2g0cdx.default')
        # browser = webdriver.Firefox(fp)
        browser = webdriver.Firefox()

        browser.get(\
                "https://www.youtube.com/results?search_query="\
                + cur.fetchone()[0])


        try:
            first_video = browser.find_element_by_id("video-title")
        except selenium.common.exceptions.NoSuchElementException:
            first_video = browser.find_element_by_class("yt-uix-tile-link")

        # print(first_video.get_attribute('href'))
        fv_link = first_video.get_attribute('href')
        # cur.execute("""INSERT INTO playlist_youtube(song_id, youtube_link)
                        # VALUES(?, ?)
                        # """, (i, fv_link))
        cur.execute("""UPDATE playlist_youtube SET youtube_link=? WHERE song_id=?""", (fv_link, i))
        # first_video.click()

        cur.execute("""SELECT playlist.song_name, youtube_link FROM playlist_youtube
                        JOIN playlist 
                        ON playlist_youtube.song_id=playlist.song_id
                        WHERE playlist_youtube.song_id=?""", (i, ))

        conn.commit()
        print(cur.fetchone())
        i+=1
        browser.close()

    conn.close()

