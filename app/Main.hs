{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Yesod hiding (count)
import qualified Web.ClientSession as CS
import Configuration.Dotenv (loadFile, defaultConfig)
import qualified Misc.Types as Types
import qualified Misc.Spotify as SP
import Misc.Sort
import System.Environment ( getEnv )
import Data.Text.Encoding (encodeUtf8, decodeLatin1)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Text.Regex.PCRE((=~))
import Text.Read
import Web.Browser
import Control.Concurrent
import GHC.IO (catchAny)
import Control.Exception (Exception)

data PlaylistSorter = PlaylistSorter
mkYesod "PlaylistSorter" [parseRoutes|
    /sorter             SorterR             GET
    /sorter/result      SorterResultR       GET
    /sorter/setup       SorterSetupR        GET POST
    /sorter/reshuffle   SorterReshuffleR    GET
    /sorter/left        SorterLeftR         POST
    /sorter/right       SorterRightR        POST
|]
instance Yesod PlaylistSorter where
    makeSessionBackend _ = sslOnlySessions $
        fmap Just $ defaultClientSessionBackend 120 "mykey.aes"
    yesodMiddleware = (sslOnlyMiddleware 120) . defaultYesodMiddleware

-- Forms
instance RenderMessage PlaylistSorter FormMessage where
    renderMessage _ _ = defaultFormMessage

data PlaylistSortParams = PlaylistSortParams {
        playlistUrl :: T.Text,
        artistRegex :: T.Text,
        nameRegex   :: T.Text
    } deriving Show

playlistSortParamsForm :: Html -> MForm Handler (FormResult PlaylistSortParams, Widget)
playlistSortParamsForm = renderDivs $ PlaylistSortParams
    <$> areq textField "Playlist URL (must be public) " Nothing
    <*> areq textField "Artist's name should match " (Just ".*")
    <*> areq textField "Song name should match " (Just ".*")

-- The GET handler displays the form
getSorterSetupR :: Handler Html
getSorterSetupR = do
    (widget, enctype) <- generateFormPost playlistSortParamsForm
    defaultLayout $ do
        y <- getSession
        liftIO $ print y
        
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
            clientId     <- liftIO $ getEnv "CLIENT_ID"
            clientSecret <- liftIO $ getEnv "CLIENT_SECRET"

            let playlistId = encodeUtf8 . parsePlaylistInput $ playlistUrl playlistSortParams
            token  <- liftIO $ SP.authWithClientCredentials clientId clientSecret
            name   <- liftIO $ maybe (return Nothing) (`SP.getPlaylistName`  playlistId) token
            tracks <- liftIO $ maybe (return [])      (`SP.getPlaylistItems` playlistId) (name >> token) -- fail if name failed
                >>= shuffle . map Types.playlistItemTrack

            case name of
                Just _ ->  return ()
                Nothing -> redirect SorterSetupR

            -- verify regex...
            let handler :: Exception e => e -> IO (Maybe String)
                handler _ = return Nothing
            let tryRegex :: String -> IO (Maybe String)
                tryRegex regex = catchAny (do
                    let test = "" :: String
                    let _ = (test =~ regex) :: Bool
                    return (Just regex)) handler
            let matchEnds :: String -> String
                matchEnds regex = "^" <> regex <> "$"

            artistRegex' <- liftIO (matchEnds . fromMaybe ".*" <$> tryRegex (T.unpack $ artistRegex playlistSortParams))
            nameRegex'   <- liftIO (matchEnds . fromMaybe ".*" <$> tryRegex (T.unpack $ nameRegex   playlistSortParams))

            let tracks' = filter
                    (\track ->
                        any (\artist -> T.unpack (Types.artistName artist) =~ artistRegex') (Types.trackArtists track)
                     && T.unpack (Types.trackName track) =~ nameRegex'
                    ) tracks
            
            y <- getSession
            liftIO $ print y

            case name of
                Just playlistName -> do
                    showAndSetSession "currPlaylist" (playlistId, playlistName)
                    showAndSetSession "matchNumber"  1
                    showAndSetSession "mergeSort"    (initialMergeSortState tracks')
                    showAndSetSession "judgements"   ([] :: [(Types.Track, Types.Track)])
                Nothing -> return ()

            y <- getSession
            liftIO $ print y

            redirect SorterR
        _ -> redirect SorterR

-- util
updateHistory :: [(Types.Track, Types.Track)] -> Either (Types.Track, Types.Track) [Types.Track] -> Ordering -> [(Types.Track, Types.Track)]
updateHistory history (Right _) _ = history
updateHistory history (Left (greater, lesser)) GT = (greater, lesser):history
updateHistory history (Left (lesser, greater)) LT = (greater, lesser):history
updateHistory _ _ EQ = error "Match tie feature is not implemented"

sorterJudgementResource :: Ordering -> Handler Html
sorterJudgementResource ord = defaultLayout $ do
    maybeMatchNumber :: Maybe Int
        <- readAndLookupSession "matchNumber"
    maybeMergeState :: Maybe (Either (Types.Track, Types.Track) [Types.Track], MergeSortState Types.Track) 
        <- readAndLookupSession "mergeSort"
    maybeJudgements :: Maybe [(Types.Track, Types.Track)]
        <- readAndLookupSession "judgements"
    
    case maybeMergeState of
        Just (nextComparison, sortState) -> do
            showAndSetSession "mergeSort"   (stepMergeSort sortState ord)
            showAndSetSession "matchNumber" (1 + fromMaybe 0 maybeMatchNumber)
            showAndSetSession "judgements"  (updateHistory (fromMaybe [] maybeJudgements) nextComparison ord)
        _ -> return ()
    
    redirect SorterR

-- resources
postSorterLeftR :: Handler Html
postSorterLeftR = sorterJudgementResource GT

postSorterRightR :: Handler Html
postSorterRightR = sorterJudgementResource LT

getSorterReshuffleR :: Handler Html
getSorterReshuffleR = defaultLayout $ do
    clientId     <- liftIO $ getEnv "CLIENT_ID"
    clientSecret <- liftIO $ getEnv "CLIENT_SECRET"

    maybeToken   <- liftIO $ SP.authWithClientCredentials clientId clientSecret
    maybeCurrPlaylist :: Maybe (Types.PlaylistId, T.Text)
        <- readAndLookupSession "currPlaylist"

    case (maybeCurrPlaylist, maybeToken) of
        (Just (playlistId, _), Just token) -> do
            tracks <- liftIO $ (`SP.getPlaylistItems` playlistId) token
                >>= shuffle . map Types.playlistItemTrack
            showAndSetSession "matchNumber" 1
            showAndSetSession "judgements"  ([] :: [(Types.Track, Types.Track)])
            showAndSetSession "mergeSort"   (initialMergeSortState tracks)
        _ -> return ()
    redirect SorterR

getSorterResultR :: Handler Html
getSorterResultR = defaultLayout $ do
    maybeMergeState :: Maybe (Either (Types.Track, Types.Track) [Types.Track], MergeSortState Types.Track) 
        <- readAndLookupSession "mergeSort"
    maybeCurrPlaylist :: Maybe (Types.PlaylistId, T.Text)
        <- readAndLookupSession "currPlaylist"

    setTitle "Playlist Sorter"
    bodyStyleWidget
    case (maybeMergeState, maybeCurrPlaylist) of
        (Just (_, MergeSortComplete sorted), Just (playlistId, playlistName)) -> foldl (>>) [whamlet|
            <h1>Playlist Sorter
            <div>
                <p>You sorted the playlist <a class="btn-link", href="https://open.spotify.com/playlist/#{decodeLatin1 playlistId}", target="_blank">#{playlistName}</a>.
                    <form class="inline-block", action="@{SorterReshuffleR}", method="GET"> 
                        <input class="btn-link inline-block" type="submit" value="Restart">
                    <a class="btn-link", href="@{SorterSetupR}">Change Playlist</a>
            <p>These are the results of the sort:
            |] $ zipWith rankedTrackWidget (map (T.pack . show) ([1..] :: [Integer])) sorted
        (Just (_, state@(MergeSortIncomplete _)), Just (playlistId, playlistName)) ->
            let ordering = currentOrderingRanks state
            in foldl (>>) [whamlet|
                <h1>Playlist Sorter
                <div>
                    <p>You are sorting the playlist <a class="btn-link", href="https://open.spotify.com/playlist/#{decodeLatin1 playlistId}", target="_blank">#{playlistName}</a>.
                        <a class="btn-link", href="@{SorterR}">Go back</a>
                        <a class="btn-link", href="@{SorterSetupR}">Change Playlist</a>
                <p>These are the tentative results of the sort:
                |] $ concatMap (\(i, x:xs) -> rankedTrackWidget (T.pack $ show i) x : map (rankedTrackWidget "--") xs) ordering
        _ -> redirect SorterSetupR

getSorterR :: Handler Html
getSorterR = defaultLayout $ do
    maybeMergeState :: Maybe (Either (Types.Track, Types.Track) [Types.Track], MergeSortState Types.Track) 
        <- readAndLookupSession "mergeSort"
    maybeMatchNumber :: Maybe Int
        <- readAndLookupSession "matchNumber"
    maybeCurrPlaylist :: Maybe (Types.PlaylistId, T.Text)
        <- readAndLookupSession "currPlaylist"
    
    liftIO $ do
        print (maybeMergeState) >> print (maybeMatchNumber) >> print (maybeCurrPlaylist)

    y <- getSession
    liftIO $ print y

    case (maybeMergeState, maybeMatchNumber, maybeCurrPlaylist) of
        (Just (nextComparison, mergeSort'), Just matchNumber', Just (playlistId, playlistName)) -> do 
            let remainingMatches = estimateComparisonsLeft mergeSort'
            let percentComplete = div (100 * (matchNumber' - 1)) (fromIntegral remainingMatches + (matchNumber' - 1))

            setTitle "Playlist Sorter"
            bodyStyleWidget
            let leftPageTop = toWidget [whamlet|
                <h1>Playlist Sorter
                <div>
                    <p class="inline-block">You are sorting the playlist <a class="btn-link", href="https://open.spotify.com/playlist/#{decodeLatin1 playlistId}", target="_blank">#{playlistName}</a>.
                    <form class="inline-block", action="@{SorterReshuffleR}", method="GET"> 
                        <input class="btn-link inline-block" type="submit" value="Restart">
                    <a class="btn-link", href="@{SorterSetupR}">Change Playlist</a>
                <p>Match #{matchNumber'}. Progress #{percentComplete}% (approximately #{remainingMatches} comparisons left)
                <br>
            |] :: WidgetFor PlaylistSorter ()
            let leftPageBottom = case nextComparison of
                    Right _ -> redirect SorterResultR
                    Left comparison -> trackComparisonWidget comparison

            toWidget [whamlet|
                <div style="width: 65%; float:left;">
                    ^{leftPageTop}
                    ^{leftPageBottom}
                <div style="width: 35%; float:right;">
                    <h3>Win-loss records
                    <div style="height: 400px; overflow-y: scroll;">
                        ^{leaderboardWidget}
            |]
        _ -> redirect SorterSetupR

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

rankedTrackWidget :: T.Text -> Types.Track -> WidgetFor PlaylistSorter ()
rankedTrackWidget rank track = do
    toWidget [whamlet|
        <p>#{rank} | #{Types.trackName track}
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
    x <- readAndLookupSession "judgements" -- :: Maybe [(Types.Track, Types.Track)]
    let judgements' = fromMaybe [] x
    let judgementItemWidget (greater, lesser) = do
          toWidget [whamlet| <p>#{Types.trackName greater} > #{Types.trackName lesser} |]
    foldl (>>) [whamlet||] $ map judgementItemWidget judgements'

leaderboardWidget :: WidgetFor PlaylistSorter ()
leaderboardWidget = do
    x <- readAndLookupSession "judgements" -- :: Maybe [(Types.Track, Types.Track)]
    let judgements' = fromMaybe [] x
    let combiner (trackLeftId, trackRightId) =
          Map.insertWith f trackLeftId  (1, 0) .
          Map.insertWith f trackRightId (0, 1)
            where f (w, l) (dw, dl) = (w+dw, l+dl)
    let winLossMap = foldl (flip combiner) Map.empty judgements' :: Map Types.Track (Int, Int)
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
    loadFile defaultConfig
    _ <- forkIO (do
        _ <- threadDelay 1000
        () <$ openBrowser "http://localhost:3000/sorter")
    putStrLn "Starting site"
    warp 3000 $ PlaylistSorter

parsePlaylistInput :: T.Text -> T.Text
parsePlaylistInput input =
    let str   = last (T.splitOn "playlist/" input)
        str'  = last (T.splitOn "playlist:" str)
        str'' = head (T.splitOn "?" str')
    in str''

-- serialization
showAndSetSession :: (MonadHandler m, Show a) => T.Text -> a -> m ()
showAndSetSession name value = setSession name (T.pack $ show value)

readAndLookupSession :: (MonadHandler m, Read a) => T.Text -> m (Maybe a)
readAndLookupSession name = do
    stored <- lookupSession name
    return (stored >>= readMaybe . T.unpack)
