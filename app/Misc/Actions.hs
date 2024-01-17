{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}

module Misc.Actions where

import Network.HTTP.Simple
import Network.HTTP.Client ( RequestBody(RequestBodyBS) )
import qualified Data.Text       as T
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson ( Object, Value(String) )
import qualified Data.Aeson.KeyMap as KM
import Misc.Types as Types
import System.Process ( readProcess )

-- Utility

partitionSongsIntoBatches :: Int -> [T.Text] -> [[T.Text]]
partitionSongsIntoBatches _ [] = []
partitionSongsIntoBatches n songs = take n songs : partitionSongsIntoBatches n (drop n songs)

-- I don't know hashmap in haskell
getKey :: Int -> T.Text
getKey 0 = "C"
getKey 1 = "C# or Db"
getKey 2 = "D"
getKey 3 = "D# or Eb"
getKey 4 = "E"
getKey 5 = "F"
getKey 6 = "F# or Gb"
getKey 7 = "G"
getKey 8 = "G# or Ab"
getKey 9 = "A"
getKey 10 = "A# or Bb"
getKey 11 = "B"
getKey _ = "Unknown"

spotifyDefaultRequest :: BS.ByteString -> BS.ByteString -> Request
spotifyDefaultRequest requestMethod token = setRequestMethod requestMethod
    $ setRequestBearerAuth token
    $ setRequestHost "api.spotify.com"
    $ setRequestPort 443
    $ setRequestSecure True
    $ defaultRequest

-- Authentication

authClientCredentialsRequest :: BS.ByteString -> BS.ByteString -> Request
authClientCredentialsRequest clientId clientSecret = setRequestMethod "POST"
    $ setRequestHost "accounts.spotify.com"
    $ setRequestPath "/api/token"
    $ setRequestPort 443
    $ setRequestSecure True
    $ setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"]
    $ setRequestQueryString [
        ("grant_type", Just "client_credentials"),
        ("client_id", Just clientId),
        ("client_secret", Just clientSecret)
    ]
    $ defaultRequest

authWithScopeRequest :: String -> String -> String -> String -> String -> IO String
authWithScopeRequest username clientId clientSecret scope redirectUri
    = readProcess "python3" ["auth.py", username, clientId, clientSecret, scope, redirectUri] []

-- Web API

getPlaylistItems :: BS.ByteString -> BS.ByteString -> IO [T.Text]
getPlaylistItems playlistId = getPlaylistItems_ (Just playlistId) 0

getPlaylistItems_ :: Maybe BS.ByteString -> Int -> BS.ByteString -> IO [T.Text]
getPlaylistItems_ Nothing _ _ = do
    return []
getPlaylistItems_ (Just playlistId) offset token = do
    let request
         = setRequestQueryString [
             ("fields", Just "uri,next,items.track.id"),
             ("offset", Just ((encodeUtf8 . T.pack . show) offset))
           ]
         $ setRequestPath ("/v1/playlists/" <> playlistId <> "/tracks")
         $ spotifyDefaultRequest "GET" token

    response <- httpJSON request :: IO (Response Playlist)
    let playlist = getResponseBody response
    let list = map (Types.id . track) (playlist_items playlist)
    nextEntries <- getPlaylistItems_ (playlist_next_url playlist >> Just playlistId) (offset + length list) token
    return (list ++ nextEntries)

getBatchAudioFeatures :: [T.Text] -> BS.ByteString -> IO [AudioFeatures]
getBatchAudioFeatures trackIds token = do
    let queryValue = T.intercalate "," trackIds
    let request = setRequestQueryString [ ("ids", Just (encodeUtf8 queryValue) ) ]
            $ setRequestPath "/v1/audio-features"
            $ spotifyDefaultRequest "GET" token
    response <- httpJSON request :: IO (Response AudioAnalysis)
    let batchAudioFeatures = audio_features (getResponseBody response)
    return batchAudioFeatures

generatePlaylistFromList :: [T.Text] -> T.Text -> T.Text -> BS.ByteString -> BS.ByteString -> IO ()
generatePlaylistFromList songIds name description token userId = do
    let body = RequestBodyBS
             $ "{\"name\": \"" <> encodeUtf8 name
            <> "\", \"description\": \"" <> encodeUtf8 description <> "\"}"
    let requestCreate = setRequestBody body
            $ addRequestHeader "Content-Type" "application/json"
            $ setRequestPath ("/v1/users/" <> userId <> "/playlists")
            $ spotifyDefaultRequest "POST" token
    responseCreate <- httpJSON requestCreate :: IO (Response Object)
    let playlistIdMaybe = (KM.lookup "id" . getResponseBody) responseCreate
    case playlistIdMaybe of
        Just (String playlistId) -> do
            addSongsToPlaylist songIds playlistId token
        _ -> print ("Could not create playlist " <> name)

addSongsToPlaylist :: [T.Text] -> T.Text -> BS.ByteString -> IO ()
addSongsToPlaylist songIds playlistId token = do
    let batches = partitionSongsIntoBatches 100 songIds
    mapM_ (\batch -> addSongsToPlaylist_ batch playlistId token) batches

addSongsToPlaylist_ :: [T.Text] -> T.Text -> BS.ByteString -> IO ()
addSongsToPlaylist_ songIds playlistId token = do
    let uriString = encodeUtf8 (T.intercalate "," (map ("spotify:track:" <>) songIds))
    let requestAdd = setRequestQueryString [ ("uris", Just uriString) ]
            $ setRequestPath ("/v1/playlists/" <> encodeUtf8 playlistId <> "/tracks")
            $ spotifyDefaultRequest "POST" token
    httpBS requestAdd
    return ()
