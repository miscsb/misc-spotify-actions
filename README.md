# Playlist Sorter

[Interactive Spotify playlist sorter](https://sorter.miscsb.com). The user acts as the judge for a sequence of pairwise matches, eventually providing enough information to rank every song in a playlist.

This sorter uses bottom-up [Mergesort](https://en.wikipedia.org/wiki/Merge_sort), which has a time complexity of $O(n \log_2 n)$.

Inspired by [Touhou Project Character Sorter](https://execfera.github.io/charasort/).

### Running the website

Create a `.env` file with the following information:

```sh
# required fields
# see Spotify web API docs
CLIENT_ID="..."
CLIENT_SECRET="..."
PORT=3000
APPROOT=""
```

Then, run `cabal run` in the directory containing this file. When the web server has started (i.e. `[Info#yesod-core] Application launched` shows up in the log), open `localhost:PORT` in your browser.

### Screenshots

##### Sorter
<image src="images/example-sorter.png">

##### Results Page
<image src="images/example-result.png">
