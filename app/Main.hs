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

data PlaylistSorter = PlaylistSorter {
        matchNumber  :: IORef Integer
    ,   playlistName :: IORef String
    ,   ordering     :: IORef (MergeSortHelper Types.Track)
}
mkYesod "PlaylistSorter" [parseRoutes|
/sorter SorterR GET
/sorter/result SorterResultR GET
/sorter/left SorterLeftR POST
/sorter/right SorterRightR POST
|]
instance Yesod PlaylistSorter

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

postSorterLeftR :: Handler Html
postSorterLeftR = resourceGenerator GT

postSorterRightR :: Handler Html
postSorterRightR = resourceGenerator LT

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

    foldl (>>) firstWidget $ zipWith trackWidget [1..] (mergeSortResult mergeSort)

getSorterR :: Handler Html
getSorterR = defaultLayout $ do
    yesod <- getYesod
    count <- liftIO $ readIORef $ matchNumber yesod
    mergeSort <- liftIO $ readIORef $ ordering yesod
    playlist <- liftIO $ readIORef $ playlistName yesod

    case nextComputation mergeSort of
        Nothing -> redirect SorterResultR
        Just (trackLeft, trackRight) -> do
            setTitle "Playlist Sorter"
            toWidget [whamlet|
                <h1>Playlist Sorter
                <h2>You are sorting the playlist #{playlist}

                <p>Match #{count}
                <p>Song 1: #{Types.trackName trackLeft}
                <p>Song 2: #{Types.trackName trackRight}

                <form action="http://localhost:3000/sorter/left", method="POST">
                    <input type="submit" value="Song 1 wins">
                <form action="http://localhost:3000/sorter/right", method="POST">
                    <input type="submit" value="Song 2 wins">
            |]

trackWidget :: Integer -> Types.Track -> WidgetFor PlaylistSorter ()
trackWidget rank track = do
    toWidget
        [whamlet|
            <p>#{rank}: #{Types.trackName track}
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
    tracks <- map Types.playlistItemTrack 
        <$> SP.getPlaylistItems (encodeUtf8 $ T.pack sourcePlaylist) accessToken
    let msh = mergeSortHelper' tracks :: MergeSortHelper Types.Track

    -- initiate vars
    playlistName' <- newIORef sourcePlaylist
    matchNumber'  <- newIORef 1
    ordering'     <- newIORef msh

    -- start server and (todo) open browser
    warp 3000 $ PlaylistSorter {
        playlistName = playlistName',
        matchNumber = matchNumber', 
        ordering = ordering'
    }
