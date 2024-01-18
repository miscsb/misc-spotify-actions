{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module ExampleActions where

import qualified Data.Text       as T
import Data.Text.Encoding (encodeUtf8)
import Data.List ( groupBy, sortOn )
import qualified Misc.Types as Types
import qualified Misc.Spotify as SP
import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (getEnv)
import Control.Monad (forM_)

main :: IO ()
main = do
    loadFile defaultConfig

sortPlaylistByEnergy :: IO ()
sortPlaylistByEnergy = do
    clientId       <- getEnv "CLIENT_ID"
    clientSecret   <- getEnv "CLIENT_SECRET"
    redirectUri    <- getEnv "REDIRECT_URI"
    username       <- getEnv "USERNAME"
    sourcePlaylist <- getEnv "SOURCE_PLAYLIST"

    putStrLn "Authorizing"
    let scope = "user-library-read playlist-modify-public playlist-modify-private"
    accessToken <- fmap (encodeUtf8 . T.pack) (SP.authWithScopeRequest username clientId clientSecret scope redirectUri)

    putStrLn "Sorting playlist"
    SP.sortPlaylistOnFeature 
        Types.audioFeaturesEnergy 
        ((encodeUtf8 . T.pack) sourcePlaylist) 
        "sorted by energy level" 
        accessToken 
        ((encodeUtf8 . T.pack) username)

groupPlaylistByKey :: IO ()
groupPlaylistByKey = do
    clientId       <- getEnv "CLIENT_ID"
    clientSecret   <- getEnv "CLIENT_SECRET"
    redirectUri    <- getEnv "REDIRECT_URI"
    username       <- getEnv "USERNAME"
    sourcePlaylist <- getEnv "SOURCE_PLAYLIST"

    -- Get token with playlist read permissions. Calls Python script.
    putStrLn "Authorizing"
    let scope = "user-library-read playlist-modify-public playlist-modify-private"
    accessToken <- fmap (encodeUtf8 . T.pack) (SP.authWithScopeRequest username clientId clientSecret scope redirectUri)

    -- Get tracks to sort
    putStrLn "Collecting tracks"
    playlistItems <- SP.getPlaylistItems ((encodeUtf8 . T.pack) sourcePlaylist) accessToken

    -- Get tracks' audio analyses
    putStrLn "Analyzing tracks"
    audioFeatures <- SP.getAudioFeatures playlistItems accessToken

    -- Group tracks into keys and sort by tempo
    putStrLn "Sorting tracks"
    let grouped = Data.List.groupBy 
            (\ a b -> Types.audioFeaturesKey a == Types.audioFeaturesKey b) 
            $ sortOn Types.audioFeaturesKey audioFeatures
    let groupedSorted = map (sortOn Types.audioFeaturesTempo) grouped
    let byKey = map 
            (\xs -> ((SP.getKey . Types.audioFeaturesKey) (head xs), map Types.audioFeaturesId xs)) 
            groupedSorted

    -- Create playlists for each track
    putStrLn "Creating playlists"
    forM_ byKey (\(songKey, ids) -> 
        SP.generatePlaylistFromList ids 
            ("Key of " <> songKey) 
            ("Songs that Spotify says are in " <> songKey <> ".") 
            accessToken 
            ((encodeUtf8 . T.pack) username))
