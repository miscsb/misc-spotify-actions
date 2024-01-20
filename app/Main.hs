{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Data.Text.Encoding (encodeUtf8, decodeLatin1)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Web.Browser
import Control.Concurrent

data PlaylistSorter = PlaylistSorter {
        matchNumber  :: IORef Integer
    ,   currPlaylist :: IORef (Types.PlaylistId, T.Text)
    ,   compsTotal   :: IORef Integer
    ,   ordering     :: IORef (MergeSortHelper Types.Track)
    ,   judgements   :: IORef [(Types.Track, Types.Track)]
}
mkYesod "PlaylistSorter" [parseRoutes|
    /sorter             SorterR             GET
    /sorter/result      SorterResultR       GET
    /sorter/setup       SorterSetupR        GET POST
    /sorter/reshuffle   SorterReshuffleR    POST
    /sorter/left        SorterLeftR         POST
    /sorter/right       SorterRightR        POST
|]
instance Yesod PlaylistSorter
instance RenderMessage PlaylistSorter FormMessage where
    renderMessage _ _ = defaultFormMessage

data PlaylistSortParams = PlaylistSortParams {
        playlistUrl :: T.Text
    } deriving Show

playlistSortParamsForm :: Html -> MForm Handler (FormResult PlaylistSortParams, Widget)
playlistSortParamsForm = renderDivs $ PlaylistSortParams
    <$> areq textField "Playlist URL (must be public)" Nothing

-- The GET handler displays the form
getSorterSetupR :: Handler Html
getSorterSetupR = do
    (widget, enctype) <- generateFormPost playlistSortParamsForm
    defaultLayout $ do
        setTitle "Playlist Sorter"
        bodyStyleWidget
        [whamlet|
            <h1>Playlist Sorter
            <form method=post action=@{SorterSetupR} enctype=#{enctype}>
                ^{widget}
                <button class="btn-link">Let's Sort!
        |]

-- The POST handler processes the form. If it is successful, it displays the
-- parsed person. Otherwise, it displays the form again with error messages.
postSorterSetupR :: Handler Html
postSorterSetupR = do
    ((result, _), _) <- runFormPost playlistSortParamsForm
    case result of
        FormSuccess playlistSortParams -> do
            yesod <- getYesod
            clientId     <- liftIO $ getEnv "CLIENT_ID"
            clientSecret <- liftIO $ getEnv "CLIENT_SECRET"
            accessTokenMaybe <- liftIO $ SP.authWithClientCredentials clientId clientSecret
            case accessTokenMaybe of
                Just accessToken -> do
                    let playlistId = encodeUtf8 . parsePlaylistInput $ playlistUrl playlistSortParams
                    playlistNameMaybe <- liftIO $ SP.getPlaylistName accessToken playlistId
                    case playlistNameMaybe of
                        Just playlistName -> do
                            tracks <- liftIO $ (=<<) shuffle (map Types.playlistItemTrack
                                <$> SP.getPlaylistItems accessToken playlistId)
                            let msh = mergeSortHelper' tracks :: MergeSortHelper Types.Track
                            _ <- liftIO $ atomicModifyIORef (currPlaylist yesod) ((playlistId, playlistName),)
                            _ <- liftIO $ atomicModifyIORef (matchNumber  yesod) (1,)
                            _ <- liftIO $ atomicModifyIORef (compsTotal   yesod) (2 + estimateRemainingComparisonCount msh,)
                            _ <- liftIO $ atomicModifyIORef (ordering     yesod) (msh,)
                            _ <- liftIO $ atomicModifyIORef (judgements   yesod) ([],)
                            redirect SorterR
                        _ -> redirect SorterR
                _ -> redirect SorterR
        _ -> redirect SorterR

-- state mutators
incCount :: (Num a, Show a) => IORef a -> IO a
incCount counter = atomicModifyIORef counter (\c -> (c+1, c))

updateOrdering :: Ordering -> IORef (MergeSortHelper a) -> IO (MergeSortHelper a)
updateOrdering ord helper = atomicModifyIORef helper (\h -> (updateMergeSort ord h, h))

updateHistory :: Maybe (Types.Track, Types.Track) -> Ordering -> IORef [(Types.Track, Types.Track)] -> IO [(Types.Track, Types.Track)]
updateHistory Nothing _ history = atomicModifyIORef history (\h -> (h, h))
updateHistory (Just (greater, lesser)) GT history = atomicModifyIORef history (\h -> ((greater, lesser):h, h))
updateHistory (Just (lesser, greater)) LT history = atomicModifyIORef history (\h -> ((greater, lesser):h, h))
updateHistory _ EQ _ = error "Match tie feature is not implemented"

resourceGenerator :: Ordering -> Handler Html
resourceGenerator ord = defaultLayout $ do
    yesod <- getYesod
    prev <- liftIO $ readIORef $ ordering yesod
    _ <- liftIO $ updateHistory (nextComparison prev) ord $ judgements yesod
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
    mergeSort  <- liftIO $ readIORef $ ordering yesod
    mergeSort' <- liftIO $ mergeSortHelper' <$> shuffle (mergeSortResult mergeSort)
    _ <- liftIO $ atomicModifyIORef (ordering    yesod) (mergeSort',)
    _ <- liftIO $ atomicModifyIORef (matchNumber yesod) (1,)
    _ <- liftIO $ atomicModifyIORef (judgements  yesod) ([],)
    redirect SorterR

getSorterResultR :: Handler Html
getSorterResultR = defaultLayout $ do
    yesod <- getYesod
    mergeSort <- liftIO $ readIORef $ ordering yesod
    (playlistId, playlistName) <- liftIO $ readIORef $ currPlaylist yesod

    setTitle "Playlist Sorter"
    bodyStyleWidget
    let firstWidget = toWidget [whamlet|
        <h1>Playlist Sorter
        <div>
            <p>You sorted the playlist <a class="btn-link", href="https://open.spotify.com/playlist/#{decodeLatin1 playlistId}", target="_blank">#{playlistName}</a>.
            <form class="inline-block", action="@{SorterReshuffleR}", method="POST"> 
                <input class="btn-link inline-block" type="submit" value="Restart?">
            <a class="btn-link", href="@{SorterSetupR}">Change Playlist?</a>
        <p>These are the results of the sort:
    |]
    foldl (>>) firstWidget $ zipWith rankedTrackWidget [1..] (mergeSortResult mergeSort)

getSorterR :: Handler Html
getSorterR = defaultLayout $ do
    yesod <- getYesod
    count        <- liftIO $ readIORef $ matchNumber yesod
    mergeSort    <- liftIO $ readIORef $ ordering yesod
    totalMatches <- liftIO $ readIORef $ compsTotal yesod
    (playlistId, playlistName) <- liftIO $ readIORef $ currPlaylist yesod

    let remainingMatches = estimateRemainingComparisonCount mergeSort
    let percentComplete = div (100 * (totalMatches - remainingMatches)) totalMatches

    setTitle "Playlist Sorter"
    bodyStyleWidget
    let leftPageTop = toWidget [whamlet|
        <h1>Playlist Sorter
        <div>
            <p class="inline-block">You are sorting the playlist <a class="btn-link", href="https://open.spotify.com/playlist/#{decodeLatin1 playlistId}", target="_blank">#{playlistName}</a>.
            <form class="inline-block", action="@{SorterReshuffleR}", method="POST"> 
                <input class="btn-link inline-block" type="submit" value="Restart?">
            <a class="btn-link", href="@{SorterSetupR}">Sort Different Playlist?</a>
        <p>Match #{count}. Progress #{percentComplete}% (approximately #{remainingMatches} comparisons left)
        <br>
    |] :: WidgetFor PlaylistSorter ()
    let leftPageBottom = case nextComparison mergeSort of
          Nothing -> redirect SorterResultR
          Just comparison -> trackComparisonWidget comparison

    toWidget [whamlet|
        <div style="width: 65%; float:left;">
            ^{leftPageTop}
            ^{leftPageBottom}
        <div style="width: 35%; float:right;">
            <h3>Win-loss records
            <div style="height: 400px; overflow-y: scroll;">
                ^{leaderboardWidget}
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
        .button:hover
            background-color: white
            transition: 0.5s
        .custom-button
            padding: 0px
            width: 200px
            height: 80px
        .btn-link
            border: none
            outline: none
            background: none
            cursor: pointer
            color: black
            padding: 0px
            text-decoration: underline
            font-family: inherit
            font-size: inherit
        .grid-container-1
            display: inline-grid
            grid-template-columns: auto
            align-items: baseline
        .grid-container-2
            display: inline-grid
            grid-template-columns: auto auto
            align-items: baseline
        .grid-container-3
            display: inline-grid
            grid-template-columns: auto auto auto
            align-items: baseline
        .grid-container-4
            display: inline-grid
            grid-template-columns: auto auto auto auto
            align-items: flex-start
        .grid-item
            width: 200px
            height: 200px
        .grid-item-2
            width: 200px
            height: 100px
        .inline-block
            display: inline-block
        .padded
            padding-right: 40px
            padding-left: 40px
            padding-bottom: 20px
        .track-text
            width: 80%
            text-align: left
            white-space: normal
            margin-left: 20px
            margin-right: 20px
        .h-fit-to-parent
            width: 100%
    |]

rankedTrackWidget :: Integer -> Types.Track -> WidgetFor PlaylistSorter ()
rankedTrackWidget rank track = do
    toWidget [whamlet|
        <p>#{rank}: #{Types.trackName track}
    |]

trackAuthorWidget :: Types.Track -> WidgetFor PlaylistSorter ()
trackAuthorWidget track = do
    let artistName = (Types.artistName . head . Types.trackArtists) track
    toWidget [whamlet|
        <p class="track-text">#{Types.trackName track}
        <p class="track-text">by #{artistName}
    |]

trackAudioWidget :: Types.Track -> WidgetFor PlaylistSorter ()
trackAudioWidget track = do
    let audioUrl = Data.Maybe.fromMaybe "" (Types.trackPreviewUrl track)
    toWidget [whamlet|
        <audio class="h-fit-to-parent" controls> <source src="#{audioUrl}">
    |]

trackImageWidget :: Types.Track -> WidgetFor PlaylistSorter ()
trackImageWidget track = do
    let imageUrl = case (Types.albumImages . Types.trackAlbum) track of
          [] -> ""
          (image:_) -> Types.imageUrl image
    toWidget [whamlet|
        <img src="#{imageUrl}" alt="#{Types.trackName track}", height="200", width="200">
    |]

trackComparisonWidget :: (Types.Track, Types.Track) -> WidgetFor PlaylistSorter ()
trackComparisonWidget (trackLeft, trackRight) = do
    toWidget [whamlet|
        <div class="grid-container-4">
            <div class="grid-item">
                ^{trackImageWidget  trackLeft}
            <div class="grid-item">
                ^{trackAuthorWidget trackLeft}
            <div class="grid-item">
                ^{trackAuthorWidget trackRight}
            <div class="grid-item">
                ^{trackImageWidget  trackRight}
        <div class="grid-container-4">
            <div class="grid-item-2">
                ^{trackAudioWidget  trackLeft}
            <div class="grid-item-2">
                <form action="@{SorterLeftR}", method="POST">
                    <input class="inline-block button custom-button", type="submit" value="#{Types.trackName trackLeft} wins">
            <div class="grid-item-2">
                <form action="@{SorterRightR}", method="POST">
                    <input class="inline-block button custom-button", type="submit" value="#{Types.trackName trackRight} wins">
            <div class="grid-item-2">
                ^{trackAudioWidget trackRight}
    |]

judgementHistoryWidget :: WidgetFor PlaylistSorter ()
judgementHistoryWidget = do
    yesod <- getYesod
    judgementItems <- liftIO $ readIORef $ judgements yesod
    let judgementItemWidget (greater, lesser) = do
          toWidget [whamlet| <p>#{Types.trackName greater} > #{Types.trackName lesser} |]
    foldl (>>) [whamlet||] $ map judgementItemWidget judgementItems

leaderboardWidget :: WidgetFor PlaylistSorter ()
leaderboardWidget = do
    yesod <- getYesod
    judgementItems <- liftIO $ readIORef $ judgements yesod
    let combiner (trackLeftId, trackRightId) =
          Map.insertWith f trackLeftId  (1, 0) .
          Map.insertWith f trackRightId (0, 1)
            where f (w, l) (dw, dl) = (w+dw, l+dl)
    let winLossMap = foldl (flip combiner) Map.empty judgementItems :: Map Types.Track (Int, Int)
    let comparator (_, (winL, lossL)) (_, (winR, lossR))
          | winL - lossL > winR - lossR = LT
          | winL - lossL < winR - lossR = GT
          | winL > winR = LT
          | winL < winR = GT
          | otherwise = EQ
    let winLossList = List.sortBy comparator (Map.toList winLossMap) :: [(Types.Track, (Int, Int))]
    let leaderboardItemWidget (track, (wins, losses)) = do
          toWidget [whamlet| <p>#{wins}W / #{losses}L &nbsp;&nbsp;&nbsp;&nbsp; #{Types.trackName track} |]
    foldl (>>) [whamlet||] $ map leaderboardItemWidget winLossList

main :: IO ()
main = do
    -- load environment variables
    loadFile defaultConfig
    clientId       <- getEnv "CLIENT_ID"
    clientSecret   <- getEnv "CLIENT_SECRET"
    sourcePlaylist <- getEnv "SOURCE_PLAYLIST" -- default playlist

    -- authorize
    putStrLn "Authorizing"
    accessTokenMaybe <- SP.authWithClientCredentials clientId clientSecret
    let accessToken = fromMaybe (error "could not authenticate") accessTokenMaybe

    -- load playlist
    putStrLn "Loading playlist"
    tracks <- (=<<) shuffle (map Types.playlistItemTrack
        <$> SP.getPlaylistItems accessToken (encodeUtf8 $ T.pack sourcePlaylist))
    let msh = mergeSortHelper' tracks :: MergeSortHelper Types.Track


    -- initiate vars
    putStrLn "Intiating variables"
    currPlaylist' <- newIORef ((encodeUtf8 . T.pack) sourcePlaylist, "Today's Top Hits")
    matchNumber'  <- newIORef 1
    compsTotal'   <- newIORef (2 + estimateRemainingComparisonCount msh)
    ordering'     <- newIORef msh
    judgements'   <- newIORef []

    _ <- forkIO (do
        _ <- threadDelay 1000
        () <$ openBrowser "http://localhost:3000/sorter")

    -- start site and (todo) open browser
    putStrLn "Starting site"
    warp 3000 $ PlaylistSorter {
        currPlaylist = currPlaylist',
        matchNumber  = matchNumber',
        compsTotal   = compsTotal',
        ordering     = ordering',
        judgements   = judgements'
    }

parsePlaylistInput :: T.Text -> T.Text
parsePlaylistInput input =
    let str   = last (T.splitOn "playlist/" input)
        str'  = last (T.splitOn "playlist:" str)
        str'' = head (T.splitOn "?" str')
    in str''
