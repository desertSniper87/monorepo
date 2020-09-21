# MoldYoMusic
Its a personal hobby project I use to transfer my spotify playlists to youtube playlists. I have created a [playlist](https://www.youtube.com/playlist?list=PLmmKsAGcnRgR12icdSNI_YC-Rv8StoyFL) using this. The process is not fully automated. I plan to do a django web app with spotify login with this.

### How to
When I started this project at 2014 I had little Idea on software engineering. I used [playlistify](http://www.playlistify.me) to generate a text file of playlist. Then I created a excel spreadsheet from that. After a while I Had the idea to transfer them into sqlite files using database_transfer.py. Then I used youtube_playlist_automation.py to use selenium for getting the top search result and store them in the db. And the use make_youtube_playlist.py to create youtube list urls. You can use urls to add to your playlist using [this answer](https://webapps.stackexchange.com/a/106340/182720).

If you have playlists containing less than 200 songs, use paid services like [soundiiz](https://soundiiz.com/). They use the youtube-api to quickly transfer songs.

### Todo
1. Add usage of youtube-api.
