-- This line finds all song ids, song names and youtube_links from db
-- IF yt links exists.

SELECT playlist.song_id, playlist.song_name, playlist_song_type.song_type, youtube_link FROM playlist_youtube
                        JOIN playlist, playlist_song_type 
                        ON playlist_youtube.song_id=playlist.song_id AND playlist_song_type.song_id=playlist.song_id
                        WHERE youtube_link IS NOT NULL
                        ;
