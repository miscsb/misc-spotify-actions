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
import Data.Aeson ( Value(String), decode )
import qualified Data.Aeson.KeyMap as KM
import qualified Misc.Types as Types
import System.Process ( readProcessWithExitCode )
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import Data.String (IsString)

-- Utility
createBatches :: Int -> [a] -> [[a]]
createBatches _ [] = []
createBatches n songs = take n songs : createBatches n (drop n songs)

createIndexedBatches :: Int -> [a] -> [(Int, [a])]
createIndexedBatches n = zip [0,n..] . createBatches n

getKey :: (IsString a) => Int -> a
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

spotifyDefaultRequest :: Types.AccessToken -> BS.ByteString -> Request
spotifyDefaultRequest token requestMethod = setRequestMethod requestMethod
    $ setRequestBearerAuth token
    $ setRequestHost "api.spotify.com"
    $ setRequestPort 443
    $ setRequestSecure True
    $ defaultRequest

-- authentication
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

authWithClientCredentials :: String -> String -> IO (Maybe Types.AccessToken)
authWithClientCredentials clientId clientSecret = do
    let authRequest = authClientCredentialsRequest (encodeUtf8 $ T.pack clientId) (encodeUtf8 $ T.pack clientSecret)
    response <- httpLBS authRequest
    return $ case getResponseStatusCode response of
        200 -> fmap (encodeUtf8 . Types.accessToken) (decode $ getResponseBody response)
        _ -> Nothing

authWithScope :: String -> String -> String -> String -> String -> IO (Maybe Types.AccessToken)
authWithScope username clientId clientSecret scope redirectUri = do
    processResult <- readProcessWithExitCode "python3" ["auth.py", username, clientId, clientSecret, scope, redirectUri] []
    return $ case processResult of
        (ExitSuccess, token, _) -> Just (encodeUtf8 $ T.pack token)
        (ExitFailure _, _, _) -> Nothing

-- playlist getters
getPlaylistItems :: Types.AccessToken -> Types.PlaylistId -> IO [Types.PlaylistItem]
getPlaylistItems token playlistId = getPlaylistItems_ token playlistId 0

getPlaylistItems_ :: Types.AccessToken -> Types.PlaylistId -> Int -> IO [Types.PlaylistItem]
getPlaylistItems_ token playlistId offset = do
    let request = setRequestQueryString [("offset", Just ((encodeUtf8 . T.pack . show) offset))]
          $ setRequestPath ("/v1/playlists/" <> playlistId <> "/tracks")
          $ spotifyDefaultRequest token "GET"
    playlist <- getResponseBody <$> httpJSON request :: IO Types.PlaylistItems
    let list = Types.playlistItemsItems playlist
    nextEntries <- case Types.playlistItemsNext playlist of
        Just _  -> getPlaylistItems_ token playlistId (offset + length list)
        Nothing -> return []
    return $ list ++ nextEntries

getPlaylistName :: Types.AccessToken -> Types.PlaylistId -> IO (Maybe T.Text)
getPlaylistName token playlistId = do
    let request = setRequestQueryString [("fields", Just "name")]
          $ setRequestPath ("/v1/playlists/" <> playlistId)
          $ spotifyDefaultRequest token "GET"
    response <- httpLBS request
    return $ case getResponseStatusCode response of
        200 -> case decode (getResponseBody response) >>= KM.lookup "name" of
            Just (String name) -> Just name
            _ -> Nothing
        _ -> Nothing

-- playlist mutators
addSongsToPlaylist :: Types.AccessToken -> [Types.TrackId] -> T.Text -> IO ()
addSongsToPlaylist token songIds playlistId = do
    let batches = createIndexedBatches 50 songIds
    mapM_ (\(offset, batch) -> addSongsToPlaylist_ token batch playlistId offset) batches

addSongsToPlaylist_ :: Types.AccessToken -> [Types.TrackId] -> T.Text -> Int -> IO ()
addSongsToPlaylist_ token songIds playlistId offset = do
    let uriString = encodeUtf8 (T.intercalate "," (map ("spotify:track:" <>) songIds))
    let requestAdd = setRequestQueryString [ ("uris", Just uriString), ("position", Just ((encodeUtf8 . T.pack . show) offset)) ]
          $ setRequestPath ("/v1/playlists/" <> encodeUtf8 playlistId <> "/tracks")
          $ spotifyDefaultRequest "POST" token
    _ <- httpLBS requestAdd
    return ()

-- playlist creators
generatePlaylistFromList :: Types.AccessToken -> [Types.TrackId] -> Types.UserId -> T.Text -> T.Text -> IO ()
generatePlaylistFromList token songIds userId name description = do
    let body = RequestBodyBS
          $ "{\"name\": \"" <> encodeUtf8 name
          <> "\", \"description\": \"" <> encodeUtf8 description
          <> "\", \"public\": false}"
    let requestCreate = setRequestBody body
          $ addRequestHeader "Content-Type" "application/json"
          $ setRequestPath ("/v1/users/" <> userId <> "/playlists")
          $ spotifyDefaultRequest "POST" token
    resultId <- KM.lookup "id" . getResponseBody <$> httpJSON requestCreate
    case resultId of
        Just (String playlistId) -> do
            addSongsToPlaylist token songIds playlistId
        _ -> print ("Could not create playlist " <> name)

sortPlaylistOnFeature :: Ord a => Types.AccessToken -> (Types.AudioFeatures -> a) -> Types.PlaylistId -> T.Text -> Types.UserId -> IO ()
sortPlaylistOnFeature token sorter playlistId newPlaylistName userId = do
    playlistItems <- getPlaylistItems playlistId token
    let playlistIds = map (Types.trackId . Types.playlistItemTrack) playlistItems
    audioFeatures <- getAudioFeatures token playlistIds
    let sorted = sortOn sorter audioFeatures
    generatePlaylistFromList token (map Types.audioFeaturesId sorted) userId newPlaylistName ""

-- audio analysis
getAudioFeatures :: Types.AccessToken -> [Types.TrackId] -> IO [Types.AudioFeatures]
getAudioFeatures token trackIds = do
    let batches = createBatches 50 trackIds
    concat <$> mapM (getAudioFeatures_ token) batches

getAudioFeatures_ :: Types.AccessToken -> [Types.TrackId] -> IO [Types.AudioFeatures]
getAudioFeatures_ token trackIds = do
    let queryValue = T.intercalate "," trackIds
    let request = setRequestQueryString [ ("ids", Just (encodeUtf8 queryValue) ) ]
          $ setRequestPath "/v1/audio-features"
          $ spotifyDefaultRequest "GET" token
    audioFeaturesArray <- getResponseBody <$> httpJSON request :: IO Types.AudioFeaturesArray
    return $ Types.audioFeatures audioFeaturesArray
