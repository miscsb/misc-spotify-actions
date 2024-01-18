{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}

module Misc.Spotify where

import Network.HTTP.Simple
import Network.HTTP.Client ( RequestBody(RequestBodyBS) )
import qualified Data.Text       as T
import qualified Data.ByteString as BS
import Data.List ( sortOn )
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson ( Object, Value(String) )
import qualified Data.Aeson.KeyMap as KM
import qualified Misc.Types as Types
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

getPlaylistItems :: BS.ByteString -> BS.ByteString -> IO [Types.PlaylistItem]
getPlaylistItems playlistId = getPlaylistItems_ playlistId 0

getPlaylistItems_ :: BS.ByteString -> Int -> BS.ByteString -> IO [Types.PlaylistItem]
getPlaylistItems_ playlistId offset token = do
    let request
         = setRequestQueryString [
             ("offset", Just ((encodeUtf8 . T.pack . show) offset))
           ]
         $ setRequestPath ("/v1/playlists/" <> playlistId <> "/tracks")
         $ spotifyDefaultRequest "GET" token
    response <- httpJSON request :: IO (Response Types.PlaylistItems)
    let playlist = getResponseBody response
    let list = Types.playlistItemsItems playlist
    nextEntries <- case Types.playlistItemsNext playlist of
        Just _  -> getPlaylistItems_ playlistId (offset + length list) token
        Nothing -> return []
    return (list ++ nextEntries)

getAudioFeatures :: [T.Text] -> BS.ByteString -> IO [Types.AudioFeatures]
getAudioFeatures trackIds token = do
    let batches = partitionSongsIntoBatches 50 trackIds
    batchAudioFeaturesResponses <- mapM (`getAudioFeatures_` token) batches
    return (concat batchAudioFeaturesResponses)

getAudioFeatures_ :: [T.Text] -> BS.ByteString -> IO [Types.AudioFeatures]
getAudioFeatures_ trackIds token = do
    let queryValue = T.intercalate "," trackIds
    let request = setRequestQueryString [ ("ids", Just (encodeUtf8 queryValue) ) ]
            $ setRequestPath "/v1/audio-features"
            $ spotifyDefaultRequest "GET" token
    response <- httpJSON request :: IO (Response Types.AudioFeaturesArray)
    let batchAudioFeatures = Types.audioFeatures $ getResponseBody response
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
    let batches = zip [0,50..] $ partitionSongsIntoBatches 50 songIds
    mapM_ (\(offset, batch) -> addSongsToPlaylist_ batch playlistId offset token) batches

addSongsToPlaylist_ :: [T.Text] -> T.Text -> Int -> BS.ByteString -> IO ()
addSongsToPlaylist_ songIds playlistId offset token = do
    let uriString = encodeUtf8 (T.intercalate "," (map ("spotify:track:" <>) songIds))
    let requestAdd = setRequestQueryString [ ("uris", Just uriString), ("position", Just ((encodeUtf8 . T.pack . show) offset)) ]
            $ setRequestPath ("/v1/playlists/" <> encodeUtf8 playlistId <> "/tracks")
            $ spotifyDefaultRequest "POST" token
    _ <- httpBS requestAdd
    return ()

sortPlaylistOnFeature :: Ord a => (Types.AudioFeatures -> a) -> BS.ByteString -> T.Text -> BS.ByteString -> BS.ByteString -> IO ()
sortPlaylistOnFeature sorter playlistId newPlaylistName token userId = do
    playlistItems <- getPlaylistItems playlistId token
    let playlistIds = map (Types.trackId . Types.playlistItemTrack) playlistItems
    audioFeatures <- getAudioFeatures playlistIds token
    let sorted = sortOn sorter audioFeatures
    generatePlaylistFromList (map Types.audioFeaturesId sorted) newPlaylistName "" token userId
