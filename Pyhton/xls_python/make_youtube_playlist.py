import sqlite3
from sqlite3 import Error

 
def create_connection(db_file_final_urls):
    """ create a database connection to a SQLite database """
    try:
        conn = sqlite3.connect(db_file_final_urls) #??????????
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
    i = 1      #Start ID

    song_type = "METAL"
    final_url = "https://www.youtube.com/watch_videos?video_ids="
    file_final_urls = open('final_url.txt', 'w')
    file_stayfocused_allow = open('stayfocused_wl.txt', 'w')
    file_leechblock_allow = open('leechblock_wl.txt', 'w')
    # file_final_urls.write("="*80+"\n")

    while(i<=n):
        cur.execute("""SELECT playlist_song_type.song_type, youtube_link FROM playlist_youtube
                        JOIN playlist_song_type 
                        ON playlist_youtube.song_id=playlist_song_type.song_id
                        WHERE playlist_youtube.song_id=?
                        AND youtube_link IS NOT NULL
                        AND song_type=?""", (i, song_type))
        x = cur.fetchone()
        if x is not None:
            yt_link = x[1].split("https://www.youtube.com/watch?v=")[1]
            final_url += yt_link + ','

            file_stayfocused_allow.write(x[1].split("https://")[1])
            file_stayfocused_allow.write("\n")

        if i%45==0:
            file_final_urls.write("\n\n"+"="*80+"\n\n")
            file_final_urls.write(final_url)
            final_url = "https://www.youtube.com/watch_videos?video_ids="

        conn.commit()
        i+=1

    conn.close()

