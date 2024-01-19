{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
import Yesod hiding (count)
import Configuration.Dotenv (loadFile, defaultConfig)
import Data.IORef ( IORef, atomicModifyIORef, newIORef, readIORef )
import qualified Misc.Types as Types
import qualified Misc.Spotify as SP
import Misc.Util
    ( MergeSortHelper,
      shuffle,
      mergeSortHelper',
      updateMergeSort,
      mergeSortResult,
      nextComparison, estimateRemainingComparisonCount )
import System.Environment ( getEnv )
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

data PlaylistSorter = PlaylistSorter {
        matchNumber  :: IORef Integer
    ,   playlistName :: IORef String
    ,   compsTotal   :: IORef Integer
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
    _ <- liftIO $ atomicModifyIORef (matchNumber yesod) (\c -> (1, c))
    redirect SorterR

getSorterResultR :: Handler Html
getSorterResultR = defaultLayout $ do
    yesod <- getYesod
    mergeSort <- liftIO $ readIORef $ ordering yesod
    playlist <- liftIO $ readIORef $ playlistName yesod

    setTitle "Playlist Sorter"
    bodyStyleWidget
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
    compsTotal <- liftIO $ readIORef $ compsTotal yesod
    playlist <- liftIO $ readIORef $ playlistName yesod

    let compsRemaining = estimateRemainingComparisonCount mergeSort
    let percentComplete = div (100 * (compsTotal - compsRemaining)) compsTotal

    setTitle "Playlist Sorter"
    bodyStyleWidget
    toWidget [whamlet|
        <h1>Playlist Sorter
        <div>
            <p class="inline-block">You are sorting the playlist #{playlist}. 
            <form class="inline-block", action="http://localhost:3000/sorter/reshuffle", method="POST"> 
                <input class="btn-link inline-block" type="submit" value="Restart?">
        <p>Match #{count}. Progress #{percentComplete}% (approximately #{compsRemaining} comparisons left)
    |]
    case nextComparison mergeSort of
        Nothing -> redirect SorterResultR
        Just (trackLeft, trackRight) -> do
            trackWidget trackLeft
            trackWidget trackRight
            toWidget [whamlet|
                <br>
                <div class="judgement-bar">
                    <form class="inline-block judgement-bar-item", action="http://localhost:3000/sorter/left", method="POST">
                        <input class="inline-block button custom-button", type="submit" value="#{Types.trackName trackLeft} wins">
                    <form class="inline-block judgement-bar-item", action="http://localhost:3000/sorter/right", method="POST">
                        <input class="inline-block button custom-button", type="submit" value="#{Types.trackName trackRight} wins">
            |]

-- widgets
bodyStyleWidget :: WidgetFor PlaylistSorter ()
bodyStyleWidget = toWidget 
    [cassius|
        body
            margin: 40px
            padding: 0px
            font-family: Helvetica Neue
            background-color: white
        .button
            background-color: lightgray
            border: none
            font-family: Helvetica;
            cursor: pointer;
            padding: 20px
            white-space: normal
        .custom-button
            padding: 0px
            width: 200px
            height: 80px
        .btn-link
            border: none
            outline: none
            background: none
            cursor: pointer
            color: #000000
            padding: 0px
            text-decoration: underline
            font-family: inherit
            font-size: inherit
        .inline-block
            display: inline-block
        .padded
            padding-right: 40px
            padding-left: 40px
            padding-bottom: 20px
        .judgement-bar
            align-items: center
            display: flex
        .judgement-bar-item
            padding-left: 40px
            padding-right: 40px
        .preview
            width: 200px
        .track-text
            width: 150px
            height: 30px
            text-align: left
    |]

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
    tracks <- (=<<) shuffle (map Types.playlistItemTrack
        <$> SP.getPlaylistItems (encodeUtf8 $ T.pack sourcePlaylist) accessToken)
    let msh = mergeSortHelper' tracks :: MergeSortHelper Types.Track

    -- initiate vars
    putStrLn "Intiating variables"
    playlistName' <- newIORef sourcePlaylist
    matchNumber'  <- newIORef 1
    compsTotal'   <- newIORef (2 + estimateRemainingComparisonCount msh)
    ordering'     <- newIORef msh

    -- start site and (todo) open browser
    putStrLn "Starting site"
    warp 3000 $ PlaylistSorter {
        playlistName = playlistName',
        matchNumber = matchNumber',
        compsTotal = compsTotal',
        ordering = ordering'
    }
