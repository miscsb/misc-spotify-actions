{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Data.Text       as T
import Data.Text.Encoding (encodeUtf8)
import Data.List ( groupBy, sortOn )
import qualified Misc.Types as Types
import Misc.Actions
import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (getEnv)
import Control.Monad (forM_)

main :: IO ()
main = do
    loadFile defaultConfig
    sortPlaylistByKey

sortPlaylistByKey :: IO ()
sortPlaylistByKey = do
    clientId       <- getEnv "CLIENT_ID"
    clientSecret   <- getEnv "CLIENT_SECRET"
    redirectUri    <- getEnv "REDIRECT_URI"
    username       <- getEnv "USERNAME"
    sourcePlaylist <- getEnv "SOURCE_PLAYLIST"

    -- Get token with playlist read permissions. Calls Python script.
    putStrLn "Authorizing"
    let scope = "user-library-read playlist-modify-public playlist-modify-private"
    accessToken <- fmap (encodeUtf8 . T.pack) (authWithScopeRequest username clientId clientSecret scope redirectUri)

    -- Get tracks to sort
    putStrLn "Collecting tracks"
    playlistItems <- getPlaylistItems ((encodeUtf8 . T.pack) sourcePlaylist) accessToken

    -- Get tracks' audio analyses
    putStrLn "Analyzing tracks"
    let batches = partitionSongsIntoBatches 50 playlistItems
    batchAudioFeaturesResponses <- mapM (`getAudioFeatures` accessToken) batches
    let audioFeatures = concat batchAudioFeaturesResponses

    -- Group tracks into keys and sort by tempo
    putStrLn "Sorting tracks"
    let grouped = Data.List.groupBy (\ a b -> Types.audioFeaturesKey a == Types.audioFeaturesKey b) $ sortOn Types.audioFeaturesKey audioFeatures
    let groupedSorted = map (sortOn Types.audioFeaturesTempo) grouped
    let byKey = map (\xs -> ((getKey . Types.audioFeaturesKey) (head xs), map Types.audioFeaturesId xs)) groupedSorted

    -- Create playlists for each track
    putStrLn "Creating playlists"
    forM_ byKey (\(songKey, ids) -> 
        generatePlaylistFromList ids 
            ("Key of " <> songKey) 
            ("Songs that Spotify says are in " <> songKey <> ".") 
            accessToken 
            ((encodeUtf8 . T.pack) username))
