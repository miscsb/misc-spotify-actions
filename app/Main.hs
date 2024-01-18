{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
import Yesod hiding (count)
import Configuration.Dotenv (loadFile, defaultConfig)
import Data.IORef
import qualified Misc.Types as Types
import qualified Misc.Spotify as SP
import Misc.Util
import System.Environment
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString as BS

data PlaylistSorter = PlaylistSorter {
        matchNumber  :: IORef Integer
    ,   playlistName :: IORef String
    ,   ordering     :: IORef (MergeSortHelper Types.Track)
}
mkYesod "PlaylistSorter" [parseRoutes|
/sorter SorterR GET
/sorter/result SorterResultR GET
/sorter/reshuffle SorterReshuffleR POST
/sorter/left SorterLeftR POST
/sorter/right SorterRightR POST
|]
instance Yesod PlaylistSorter

-- state mutators
incCount :: (Num a, Show a) => IORef a -> IO a
incCount counter = atomicModifyIORef counter (\c -> (c+1, c))

updateOrdering :: Ordering -> IORef (MergeSortHelper a) -> IO (MergeSortHelper a)
updateOrdering ord helper = atomicModifyIORef helper (\h -> (updateMergeSort ord h, h))

resourceGenerator :: Ordering -> Handler Html
resourceGenerator ord = defaultLayout $ do
    yesod <- getYesod
    _ <- liftIO $ updateOrdering ord $ ordering yesod
    _ <- liftIO $ incCount $ matchNumber yesod
    redirect SorterR

-- resources
postSorterLeftR :: Handler Html
postSorterLeftR = resourceGenerator GT

postSorterRightR :: Handler Html
postSorterRightR = resourceGenerator LT

postSorterReshuffleR :: Handler Html
postSorterReshuffleR = defaultLayout $ do
    yesod <- getYesod
    mergeSort <- liftIO $ readIORef $ ordering yesod
    mergeSort' <- liftIO $ mergeSortHelper' <$> shuffle (mergeSortResult mergeSort)
    _ <- liftIO $ atomicModifyIORef (ordering yesod) (\h -> (mergeSort', h))
    _ <- liftIO $ atomicModifyIORef (matchNumber yesod) (\c -> (1, c))
    redirect SorterR

getSorterResultR :: Handler Html
getSorterResultR = defaultLayout $ do
    yesod <- getYesod
    mergeSort <- liftIO $ readIORef $ ordering yesod
    playlist <- liftIO $ readIORef $ playlistName yesod

    setTitle "Playlist Sorter"
    let firstWidget = toWidget [whamlet|
        <h1>Playlist Sorter
        <h2>You sorted the playlist #{playlist}
        <p>These are the results of the sort:
    |]
    foldl (>>) firstWidget $ zipWith rankedTrackWidget [1..] (mergeSortResult mergeSort)

getSorterR :: Handler Html
getSorterR = defaultLayout $ do
    yesod <- getYesod
    count <- liftIO $ readIORef $ matchNumber yesod
    mergeSort <- liftIO $ readIORef $ ordering yesod
    playlist <- liftIO $ readIORef $ playlistName yesod

    setTitle "Playlist Sorter"
    toWidget [whamlet|
        <h1>Playlist Sorter
        <p>You are sorting the playlist #{playlist}
        <form action="http://localhost:3000/sorter/reshuffle", method="POST">
            <input type="submit" value="Restart">
        <p><b>Match #{count}</b>
    |]
    toWidget [cassius|
        .inline-block
            display: inline-block
        .padded
            padding-right: 40px
            padding-left: 40px
            padding-bottom: 20px
        .custom-button
            width: 200px
            height: 80px
            white-space: normal
        .judgement-bar
            align-items: center
            display: flex
        .judgement-bar-item
            padding-left: 40px
            padding-right: 40px
    |]
    case nextComputation mergeSort of
        Nothing -> redirect SorterResultR
        Just (trackLeft, trackRight) -> do
            trackWidget trackLeft
            trackWidget trackRight
            toWidget [whamlet|
                <br>
                <div class="judgement-bar">
                    <form class="inline-block judgement-bar-item", action="http://localhost:3000/sorter/left", method="POST">
                        <input class="inline-block custom-button", type="submit" value="#{Types.trackName trackLeft} wins">
                    <form class="inline-block judgement-bar-item", action="http://localhost:3000/sorter/right", method="POST">
                        <input class="inline-block custom-button", type="submit" value="#{Types.trackName trackRight} wins">
            |]

-- widgets
rankedTrackWidget :: Integer -> Types.Track -> WidgetFor PlaylistSorter ()
rankedTrackWidget rank track = do
    toWidget [whamlet|
        <p>#{rank}: #{Types.trackName track}
    |]

trackWidget :: Types.Track -> WidgetFor PlaylistSorter ()
trackWidget track = do
    let imageUrl = case (Types.albumImages . Types.trackAlbum) track of
            [] -> ""
            image:_ -> Types.imageUrl image
    let audioUrl = case Types.trackPreviewUrl track of
            Nothing -> ""
            Just url -> url
    let artistName = (Types.artistName . head . Types.trackArtists) track
    toWidget [cassius|
        .preview
            width: 200px
        .track-text
            width: 150px
            height: 30px
            text-align: left
    |]
    toWidget [whamlet|
        <div class="inline-block padded">
            <img src="#{imageUrl}" alt="#{Types.trackName track}", height="200", width="200">
            <p class="track-text">#{Types.trackName track}
            <p class="track-text">by #{artistName}
            <audio class="preview" controls> <source src="#{audioUrl}">
    |]

main :: IO ()
main = do
    -- load environment variables
    loadFile defaultConfig
    clientId       <- getEnv "CLIENT_ID"
    clientSecret   <- getEnv "CLIENT_SECRET"
    redirectUri    <- getEnv "REDIRECT_URI"
    username       <- getEnv "USERNAME"
    sourcePlaylist <- getEnv "SOURCE_PLAYLIST"

    -- authorize
    putStrLn "Authorizing"
    let scope = "user-library-read"
    accessToken <- fmap (encodeUtf8 . T.pack) (SP.authWithScopeRequest username clientId clientSecret scope redirectUri)

    -- load playlist
    putStrLn "Loading playlist"
    tracks <- flip (>>=) shuffle (map Types.playlistItemTrack
        <$> SP.getPlaylistItems (encodeUtf8 $ T.pack sourcePlaylist) accessToken)
    let msh = mergeSortHelper' tracks :: MergeSortHelper Types.Track

    -- initiate vars
    putStrLn "Intiating variables"
    playlistName' <- newIORef sourcePlaylist
    matchNumber'  <- newIORef 1
    ordering'     <- newIORef msh

    -- start site and (todo) open browser
    putStrLn "Starting site"
    warp 3000 $ PlaylistSorter {
        playlistName = playlistName',
        matchNumber = matchNumber',
        ordering = ordering'
    }
