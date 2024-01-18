{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
import Yesod hiding (count)
import Configuration.Dotenv (loadFile, defaultConfig)
import Data.IORef
import qualified Data.Text as T
-- import Misc.Types (Track)
import Misc.Util
-- import Misc.Types

data PlaylistSorter = PlaylistSorter {
        matchNumber :: IORef Integer
    ,   ordering    :: IORef (MergeSortHelper Integer)
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
    let result = mergeSortResult mergeSort

    setTitle "Playlist Sorter"
    let firstWidget = toWidget [whamlet|
        <h1> Playlist Sorter
        <p>These are the results of the sort:
    |]

    foldl (>>) firstWidget $ zipWith trackWidget [1..] result

getSorterR :: Handler Html
getSorterR = defaultLayout $ do
    yesod <- getYesod

    let handleMaybePair = (\x -> case x of
            Nothing -> ("N/A", "N/A")
            Just (y, z) -> (T.pack $ show y, T.pack $ show z)
            )
    count <- liftIO $ readIORef $ matchNumber yesod
    mergeSort <- liftIO $ readIORef $ ordering yesod

    if isMergeSortComplete mergeSort
        then redirect SorterResultR
    else do
        let (compLeft, compRight) = handleMaybePair <$> nextComputation $ mergeSort
        setTitle "Playlist Sorter"
        toWidget
            [whamlet|
                <h1> Playlist Sorter
                <p>You are sorting the playlist ???

                <p>Match #{count}
                <p>Song 1: #{compLeft}
                <p>Song 2: #{compRight}

                <form action="http://localhost:3000/sorter/left", method="POST">
                    <input type="submit" value="Song 1 wins">
                <form action="http://localhost:3000/sorter/right", method="POST">
                    <input type="submit" value="Song 2 wins">
            |]

trackWidget :: Integer -> Integer -> WidgetFor PlaylistSorter ()
trackWidget rank value = do
    toWidget
        [whamlet|
            <p>#{rank}: #{value}
        |]

main :: IO ()
main = do
    loadFile defaultConfig

    matchNumber' <- newIORef 1

    ordering' <- newIORef $ mergeSortHelper' [1, 5, 2, 4, 3]

    warp 3000 $ PlaylistSorter {
        matchNumber = matchNumber'
    ,   ordering = ordering'
    }
