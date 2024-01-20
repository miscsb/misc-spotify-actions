{-# OPTIONS_GHC -Wno-unused-matches #-}
module Misc.Util where

import qualified System.Random
import Data.Array.IO hiding (newArray)
import Control.Monad

-- https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArray n xs
    forM [1..n] $ \i -> do
        j <- System.Random.randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
    where
        n = length xs
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray k = newListArray (1, k)

data MergeState a = MergeComplete [a] | MergeIncomplete ([a], [a], [a])
    deriving Show

stepMerge :: MergeState a -> Ordering -> MergeState a
stepMerge (MergeComplete xs) _ = MergeComplete xs
stepMerge ms EQ = ms
stepMerge (MergeIncomplete (left, right, temp)) GT =
    let temp' = temp ++ take 1 left
        left' = drop 1 left
    in case left' of
        [] -> MergeComplete (temp' ++ right)
        _  -> MergeIncomplete (left', right, temp')
stepMerge (MergeIncomplete (left, right, temp)) LT
    = stepMerge (MergeIncomplete (right, left, temp)) GT

currentMergeComparison :: MergeState a -> Maybe (a, a)
currentMergeComparison (MergeComplete _) = Nothing
currentMergeComparison (MergeIncomplete (left, right, _)) = Just (head left, head right)

data MergeSortState a = MergeSortComplete [a] | MergeSortIncomplete ([[a]], [[a]], MergeState a)
    deriving Show

initialMergeSortState :: [a] -> MergeSortState a
initialMergeSortState [] = MergeSortComplete []
initialMergeSortState xs = stepNextMerge $ MergeSortIncomplete ([], [[x] | x <- xs], MergeComplete [])

stepNextMerge :: MergeSortState a -> MergeSortState a
stepNextMerge s@(MergeSortComplete _) = s
stepNextMerge (MergeSortIncomplete (_, _, MergeIncomplete _)) = error "Merge is not complete."
stepNextMerge (MergeSortIncomplete (prev, next, MergeComplete combined)) =
    let prev' = prev ++ filter (not . null) [combined]
    in case (prev', next) of
        ([singleRun], []) -> MergeSortComplete singleRun
        (_, [])           -> stepNextMerge $ MergeSortIncomplete ([], prev', MergeComplete [])
        (_, [singleRun])  -> stepNextMerge $ MergeSortIncomplete (prev', [], MergeComplete singleRun)
        (_, nextL:nextR:next') -> MergeSortIncomplete (prev', next', MergeIncomplete (nextL, nextR, []))

stepMergeSort :: MergeSortState a -> Ordering -> MergeSortState a
stepMergeSort s@(MergeSortComplete _) _ = s
stepMergeSort (MergeSortIncomplete (prev, next, mergeState@(MergeIncomplete _))) ord =
    let mergeState' = stepMerge mergeState ord
    in case mergeState' of
        MergeComplete _ -> stepNextMerge (MergeSortIncomplete (prev, next, mergeState'))
        MergeIncomplete _ -> MergeSortIncomplete (prev, next, mergeState')
stepMergeSort (MergeSortIncomplete (_, _, MergeComplete _)) _ = error "Not allowed"

currentMergeSortComparison :: MergeSortState a -> Maybe (a, a)
currentMergeSortComparison (MergeSortComplete _) = Nothing
currentMergeSortComparison (MergeSortIncomplete (_, _, merge)) = currentMergeComparison merge

estimateComparisonsLeft :: MergeSortState a -> Int
estimateComparisonsLeft _ = 1 -- TODO add functionality
