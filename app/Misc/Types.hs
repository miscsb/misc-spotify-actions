{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use camelCase" #-}

module Misc.Types where

import GHC.Generics ( Generic )
import Data.Aeson
    ( genericParseJSON,
      defaultOptions,
      FromJSON(parseJSON),
      Options(fieldLabelModifier),
      ToJSON )
import qualified Data.Text as T

-- Data | Auth --
data TokenContainer = TokenContainer {
        access_token :: T.Text
    } deriving (Generic, Show)
instance ToJSON TokenContainer
instance FromJSON TokenContainer

-- Data | Playlists --
data Playlist = Playlist {
        playlist_next_url :: Maybe T.Text
    ,   playlist_items    :: [PlaylistTrack]
    } deriving (Generic, Show)
instance ToJSON Playlist
instance FromJSON Playlist where
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = body_noprefix })

data PlaylistTrack = PlaylistTrack {
        track :: TrackContainer
    } deriving (Generic, Show)
instance ToJSON PlaylistTrack
instance FromJSON PlaylistTrack

data TrackContainer = TrackContainer {
        id :: T.Text
    } deriving (Generic, Show)
instance ToJSON TrackContainer
instance FromJSON TrackContainer

-- Data | Track Analysis --
data AudioAnalysis = AudioAnalysis {
        audio_features :: [AudioFeatures]
    } deriving (Generic, Show)
instance ToJSON AudioAnalysis
instance FromJSON AudioAnalysis

data AudioFeatures = AudioFeatures {
        af_id :: T.Text
    ,   key   :: Int
    ,   tempo :: Double
    } deriving (Generic, Show)
instance ToJSON AudioFeatures
instance FromJSON AudioFeatures where
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = body_noprefix })

body_noprefix :: String -> String
body_noprefix "af_id" = "id"
body_noprefix "playlist_items" = "items"
body_noprefix "playlist_next_url" = "next"
body_noprefix s = s
