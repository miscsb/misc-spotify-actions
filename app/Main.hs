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
import Web.Cookie
import Configuration.Dotenv (loadFile, defaultConfig)
import qualified Misc.Types as Types
import qualified Misc.Spotify as SP
import Misc.Sort
import System.Environment ( getEnv )
import Data.Text.Encoding (encodeUtf8, decodeLatin1)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.IORef
import Text.Regex.PCRE((=~))
import Text.Read
import GHC.IO (catchAny)
import Control.Exception (Exception)

data PlaylistSorter = PlaylistSorter {
    playlistCache :: IORef (Map Types.PlaylistId (Map Types.TrackId Types.Track)),
    myApproot :: T.Text
}

mkYesod "PlaylistSorter" [parseRoutes|
    /               SorterHomeR      GET
    /setup          SorterSetupR     GET POST
    /sorter         SorterR          GET
    /sorter/left    SorterLeftR      POST
    /sorter/right   SorterRightR     POST
    /result         SorterResultR    GET
    /reshuffle      SorterReshuffleR GET
|]
instance Yesod PlaylistSorter where
    approot = ApprootMaster myApproot

-- State
type TrackSortState = Maybe (Either (Types.TrackId, Types.TrackId) [Types.TrackId], SortState Types.TrackId)
sortStateCookie    :: TypedCookie TrackSortState
currPlaylistCookie :: TypedCookie (Types.PlaylistId, T.Text)
matchNumberCookie  :: TypedCookie Int
judgementsCookie   :: TypedCookie [(Types.TrackId, Types.TrackId)]

sortStateCookie    = "sortState"
currPlaylistCookie = "currPlaylist"
matchNumberCookie  = "matchNumber"
judgementsCookie   = "judgements"

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

            yesod <- getYesod
            playlistCache' <- liftIO $ readIORef (playlistCache yesod)
            let newPlaylistMap = Map.fromList $ map (\track -> (Types.trackId track, track)) tracks
            let newPlaylistCache = Map.insert playlistId newPlaylistMap playlistCache'
            _ <- liftIO $ writeIORef (playlistCache yesod) newPlaylistCache

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

            let sortState = newSort (map Types.trackId tracks')

            case name of
                Just playlistName -> do
                    showAndSetCookie sortStateCookie    sortState
                    showAndSetCookie currPlaylistCookie (playlistId, playlistName)
                    showAndSetCookie matchNumberCookie  (1 :: Integer)
                    -- showAndSetCookie judgementsCookie   ([] :: [(Types.TrackId, Types.TrackId)])
                    return ()
                Nothing -> return ()
            redirect SorterR
        _ -> redirect SorterR

-- util
updateHistory :: [(Types.TrackId, Types.TrackId)] -> Either (Types.TrackId, Types.TrackId) [Types.TrackId] -> Ordering -> [(Types.TrackId, Types.TrackId)]
updateHistory history (Right _) _ = history
updateHistory history (Left (greater, lesser)) GT = (greater, lesser):history
updateHistory history (Left (lesser, greater)) LT = (greater, lesser):history
updateHistory _ _ EQ = error "Match tie feature is not implemented"

sorterJudgementResource :: Ordering -> Handler Html
sorterJudgementResource ord = defaultLayout $ do
    maybeMatchNumber :: Maybe Integer
        <- readAndLookupCookie matchNumberCookie
    maybeSortState :: TrackSortState
        <- readAndLookupCookie sortStateCookie
    case maybeSortState of
        Just (_, sortState) -> do
            showAndSetCookie sortStateCookie   (stepSort sortState ord)
            showAndSetCookie matchNumberCookie ((1 :: Integer) + fromMaybe 0 maybeMatchNumber)
        _ -> return ()

-- resources
getSorterHomeR :: Handler Html
getSorterHomeR = do
    redirect SorterSetupR

postSorterLeftR :: Handler Html
postSorterLeftR = sorterJudgementResource GT

postSorterRightR :: Handler Html
postSorterRightR = sorterJudgementResource LT

getSorterReshuffleR :: Handler Html
getSorterReshuffleR = defaultLayout $ do
    maybeSortState :: TrackSortState
        <- readAndLookupCookie sortStateCookie
    case maybeSortState of
        (Just (_, sortState)) -> do
            let currItems = concat $ currentOrdering sortState
            shuffled <- liftIO $ shuffle currItems
            let sortState' = newSort shuffled
            showAndSetCookie sortStateCookie sortState'
        Nothing -> redirect SorterSetupR
    showAndSetCookie matchNumberCookie (1 :: Integer)
    -- showAndSetCookie judgementsCookie ([] :: [(Types.TrackId, Types.TrackId)])
    redirect SorterR

getSorterResultR :: Handler Html
getSorterResultR = defaultLayout $ do
    maybeSortState :: TrackSortState
        <- readAndLookupCookie sortStateCookie
    maybeCurrPlaylist :: Maybe (Types.PlaylistId, T.Text)
        <- readAndLookupCookie currPlaylistCookie

    setTitle "Playlist Sorter"
    bodyStyleWidget
    case (maybeSortState, maybeCurrPlaylist) of
        (Just (_, SortComplete sorted), Just (playlistId, playlistName)) -> foldl (>>) [whamlet|
            <h1>Playlist Sorter
            <div>
                <p>You sorted the playlist <a class="btn-link", href="https://open.spotify.com/playlist/#{decodeLatin1 playlistId}", target="_blank">#{playlistName}</a>.
                    <a class="btn-link", href="@{SorterReshuffleR}">Restart</a>
                    <a class="btn-link", href="@{SorterSetupR}">Change Playlist</a>
            <p>These are the results of the sort:
            |] $ zipWith rankedTrackWidget (map (T.pack . show) ([1..] :: [Integer])) sorted
        (Just (_, state@(SortIncomplete _)), Just (playlistId, playlistName)) ->
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
    maybeSortState :: TrackSortState
        <- readAndLookupCookie sortStateCookie
    maybeMatchNumber :: Maybe Int
        <- readAndLookupCookie matchNumberCookie
    maybeCurrPlaylist :: Maybe (Types.PlaylistId, T.Text)
        <- readAndLookupCookie currPlaylistCookie

    case (maybeSortState, maybeMatchNumber, maybeCurrPlaylist) of
        (Just (nextComparison, mergeSort'), Just matchNumber', Just (playlistId, playlistName)) -> do
            let remainingMatches = estimateComparisonsLeft mergeSort'
            let percentComplete = div (100 * (matchNumber' - 1)) (fromIntegral remainingMatches + (matchNumber' - 1))

            setTitle "Playlist Sorter"
            bodyStyleWidget
            let leftPageTop = toWidget [whamlet|
                <h1>Playlist Sorter
                <div>
                    <p class="inline-block">You are sorting the playlist <a class="btn-link", href="https://open.spotify.com/playlist/#{decodeLatin1 playlistId}", target="_blank">#{playlistName}</a>.
                    <a class="btn-link", href="@{SorterReshuffleR}">Restart</a>
                    <a class="btn-link", href="@{SorterSetupR}">Change Playlist</a>
                <p>Match #{matchNumber'}. Progress #{percentComplete}% (approximately #{remainingMatches} comparisons left)
                <br>
            |] :: WidgetFor PlaylistSorter ()
            let leftPageBottom = case nextComparison of
                    Right _ -> redirect SorterResultR
                    Left comparison -> trackComparisonWidget comparison

            -- toWidget [whamlet|
            --     <div style="width: 65%; float:left;">
            --         ^{leftPageTop}
            --         ^{leftPageBottom}
            --     <div style="width: 35%; float:right;">
            --         <h3>Win-loss records
            --         <div style="height: 400px; overflow-y: scroll;">
            --             ^{leaderboardWidget}
            -- |]
            toWidget [whamlet|
                <div style="width: 65%; float:left;">
                    ^{leftPageTop}
                    ^{leftPageBottom}
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

rankedTrackWidget :: T.Text -> Types.TrackId -> WidgetFor PlaylistSorter ()
rankedTrackWidget rank track = do
    yesod <- getYesod
    resolvedTrack <- resolveTrack yesod track
    toWidget [whamlet|
        <p>#{rank} | #{Types.trackName resolvedTrack}
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

trackComparisonWidget :: (Types.TrackId, Types.TrackId) -> WidgetFor PlaylistSorter ()
trackComparisonWidget (trackLeft, trackRight) = do
    yesod <- getYesod
    resolvedTrackLeft  <- resolveTrack yesod trackLeft
    resolvedTrackRight <- resolveTrack yesod trackRight
    toWidget[julius|
        judgement = (resource) => {
            const xhr = new XMLHttpRequest();
            xhr.open("POST", resource);
            xhr.onload = () => {
                window.location.replace("@{SorterR}");
            };
            xhr.send();
        };
        judgeLeft  = () => { judgement("@{SorterLeftR}");  };
        judgeRight = () => { judgement("@{SorterRightR}"); };
    |]
    toWidget [whamlet|
        <div class="grid-container-4">
            <div class="grid-item">
                ^{trackImageWidget  resolvedTrackLeft}
            <div class="grid-item">
                ^{trackAuthorWidget resolvedTrackLeft}
            <div class="grid-item">
                ^{trackAuthorWidget resolvedTrackRight}
            <div class="grid-item">
                ^{trackImageWidget  resolvedTrackRight}
        <div class="grid-container-4">
            <div class="grid-item-2">
                ^{trackAudioWidget  resolvedTrackLeft}
            <div class="grid-item-2">
                <button class="inline-block button custom-button", onclick="judgeLeft()"> <p>#{Types.trackName resolvedTrackLeft}</p> </button>
            <div class="grid-item-2">
                <button class="inline-block button custom-button", onclick="judgeRight()"> <p>#{Types.trackName resolvedTrackRight}</p> </button>
            <div class="grid-item-2">
                ^{trackAudioWidget resolvedTrackRight}
    |]

judgementHistoryWidget :: WidgetFor PlaylistSorter ()
judgementHistoryWidget = do
    judgements <- fromMaybe [] <$> readAndLookupCookie judgementsCookie
    let judgementItemWidget (greater, lesser) = do
          yesod <- getYesod
          resolvedLesser  <- resolveTrack yesod lesser
          resolvedGreater <- resolveTrack yesod greater
          toWidget [whamlet| <p>#{Types.trackName resolvedGreater} > #{Types.trackName resolvedLesser} |]
    foldl (>>) [whamlet||] $ map judgementItemWidget judgements

leaderboardWidget :: WidgetFor PlaylistSorter ()
leaderboardWidget = do
    judgements <- fromMaybe [] <$> readAndLookupCookie judgementsCookie
    let combiner (trackLeftId, trackRightId) =
          Map.insertWith f trackLeftId  (1, 0) .
          Map.insertWith f trackRightId (0, 1)
            where f (w, l) (dw, dl) = (w+dw, l+dl)
    let winLossMap = foldl (flip combiner) Map.empty judgements :: Map Types.TrackId (Int, Int)
    let comparator (_, (winL, lossL)) (_, (winR, lossR))
          | winL - lossL > winR - lossR = LT
          | winL - lossL < winR - lossR = GT
          | winL > winR = LT
          | winL < winR = GT
          | otherwise = EQ
    let winLossList = List.sortBy comparator (Map.toList winLossMap) :: [(Types.TrackId, (Int, Int))]
    let leaderboardItemWidget (track, (wins, losses)) = do
          yesod <- getYesod
          resolvedTrack <- resolveTrack yesod track
          toWidget [whamlet| <p>#{wins}W / #{losses}L &nbsp;&nbsp;&nbsp;&nbsp; #{Types.trackName resolvedTrack} |]
    foldl (>>) [whamlet||] $ map leaderboardItemWidget winLossList
    return ()

resolveTrack :: (MonadHandler m) => PlaylistSorter -> Types.TrackId -> m Types.Track
resolveTrack yesod trackId = do
    maybeCurrPlaylist :: Maybe (Types.PlaylistId, T.Text)
        <- readAndLookupCookie currPlaylistCookie
    case maybeCurrPlaylist of
        Just (playlistId, _) -> do
            playlistCache' <- liftIO $ readIORef (playlistCache yesod)
            let playlist = Map.lookup playlistId playlistCache'
            let track = playlist >>= Map.lookup trackId
            case track of
                Just resolvedTrack -> return resolvedTrack
                Nothing -> error "NOOOO"
        Nothing -> error "NOOOO"

main :: IO ()
main = do
    loadFile defaultConfig
    port <- getEnv "PORT"
    myApproot' <- T.pack <$> getEnv "APPROOT"
    playlistCache' <- newIORef Map.empty
    warp (read port) $ PlaylistSorter {
        myApproot = myApproot',
        playlistCache = playlistCache'
    }

parsePlaylistInput :: T.Text -> T.Text
parsePlaylistInput input =
    let str   = last (T.splitOn "playlist/" input)
        str'  = last (T.splitOn "playlist:" str)
        str'' = head (T.splitOn "?" str')
    in str''

-- serialization
type TypedCookie a = BS.ByteString

showAndSetCookie :: (MonadHandler m, Show a) => TypedCookie a -> a -> m ()
showAndSetCookie name value = setCookie $ defaultSetCookie { setCookiePath = Just "/", setCookieName = name, setCookieValue = encodeUtf8 $ T.pack $ show value }

readAndLookupCookie :: (MonadHandler m, Read a) => TypedCookie a -> m (Maybe a)
readAndLookupCookie name = do
    stored <- lookupCookie (decodeLatin1 name)
    return (stored >>= readMaybe . T.unpack)
