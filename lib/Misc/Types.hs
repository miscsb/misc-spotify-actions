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
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Text.Casing

-- Type aliases
type AccessToken = BS.ByteString
type PlaylistId = BS.ByteString
type UserId = BS.ByteString
type TrackId = T.Text

-- Data | Auth --
data TokenContainer = TokenContainer {
        accessToken :: T.Text
    } deriving (Generic, Show)
instance ToJSON TokenContainer
instance FromJSON TokenContainer where
    parseJSON = genericParseJSON (customOptions "")

-- Playlists --
data PlaylistItems = PlaylistItems {
        playlistItemsNext   :: Maybe T.Text
    ,   playlistItemsItems :: [PlaylistItem]
    } deriving (Generic, Show)
instance ToJSON PlaylistItems
instance FromJSON PlaylistItems where
    parseJSON = genericParseJSON (customOptions "playlistItems")

data PlaylistItem = PlaylistItem {
        playlistItemAddedAt :: T.Text
        -- TODO add added_by --
    ,   playlistItemTrack :: Track
        -- TODO add EpisodeTrack suppert --
    } deriving (Generic, Show)
instance ToJSON PlaylistItem
instance FromJSON PlaylistItem where
    parseJSON = genericParseJSON (customOptions "playlistItem")

data Track = Track {
        trackAlbum       :: Album
    ,   trackArtists     :: [Artist]
    ,   trackDiscNumber  :: Int
    -- ,   trackDurationMs  :: Int
    -- ,   trackExplicit    :: Bool
    -- ,   trackHref        :: T.Text
    ,   trackId          :: TrackId
    -- TODO add is_playable, linked_from, restrictions --
    ,   trackName        :: T.Text
    -- ,   trackPopularity  :: Double
    ,   trackPreviewUrl  :: Maybe T.Text
    -- ,   trackTrackNumber :: Int
    ,   trackUri         :: T.Text
    } deriving (Generic, Show, Read, Ord, Eq)
instance ToJSON Track
instance FromJSON Track where
    parseJSON = genericParseJSON (customOptions "track")

data Artist = Artist {
        artistName       :: T.Text
    ,   artistId         :: T.Text
    -- TODO add other fields --
    } deriving (Generic, Show, Read, Ord, Eq)
instance ToJSON Artist
instance FromJSON Artist where
    parseJSON = genericParseJSON (customOptions "artist")

data Album = Album {
        albumImages     :: [Image]
    ,   albumName       :: T.Text
    ,   albumId         :: T.Text
    -- TODO add other fields --
    } deriving (Generic, Show, Read, Ord, Eq)
instance ToJSON Album
instance FromJSON Album where
    parseJSON = genericParseJSON (customOptions "album")

data Image = Image {
        imageUrl    :: T.Text
    ,   imageHeight :: Maybe Int
    ,   imageWidth  :: Maybe Int
    } deriving (Generic, Show, Read, Ord, Eq)
instance ToJSON Image
instance FromJSON Image where
    parseJSON = genericParseJSON (customOptions "image")

-- Audio Analyses --
data AudioFeatures = AudioFeaturesObject {
        audioFeaturesAcousticness     :: Double
    ,   audioFeaturesAnalysisUrl      :: T.Text
    ,   audioFeaturesDanceability     :: Double
    ,   audioFeaturesDurationMs       :: Int
    ,   audioFeaturesEnergy           :: Double
    ,   audioFeaturesId               :: TrackId
    ,   audioFeaturesInstrumentalness :: Double
    ,   audioFeaturesKey              :: Int
    ,   audioFeaturesLiveness         :: Double
    ,   audioFeaturesLoudness         :: Double
    ,   audioFeaturesMode             :: Int
    ,   audioFeaturesSpeechiness      :: Double
    ,   audioFeaturesTempo            :: Double
    ,   audioFeaturesTimeSignature    :: Int
    ,   audioFeaturesTrackHref        :: T.Text
    ,   audioFeaturesUri              :: T.Text
    ,   audioFeaturesValence          :: Double
    } deriving (Generic, Show)
instance ToJSON AudioFeatures
instance FromJSON AudioFeatures where
    parseJSON = genericParseJSON (customOptions "audioFeatures")

data AudioFeaturesArray = AudioFeaturesArray {
    audioFeatures :: [AudioFeatures]
} deriving (Generic, Show)
instance FromJSON AudioFeaturesArray where
    parseJSON = genericParseJSON (customOptions "")

removePrefixSnake :: String -> String -> String
removePrefixSnake prefix = Text.Casing.quietSnake . drop (length prefix)

customOptions :: String -> Options
customOptions prefix = defaultOptions { fieldLabelModifier = removePrefixSnake prefix }
