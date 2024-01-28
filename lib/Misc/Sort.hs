{-# OPTIONS_GHC -Wno-unused-matches #-}
module Misc.Sort (
    stepSort,
    judge,
    newSort,
    estimateComparisonsLeft,
    currentOrdering,
    currentOrderingRanks,
    SortState(SortComplete, SortIncomplete)
) where

import qualified System.Random
import Data.Array.IO hiding (newArray)
import Control.Monad
import Data.List (transpose, singleton)
import Control.Monad.Trans.State

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
    deriving (Show, Read)

newMerge :: [a] -> [a] -> MergeState a
newMerge currL@(headL:_) currR@(headR:_) = MergeIncomplete (currL, currR, [])
newMerge merged [] = MergeComplete merged
newMerge [] merged = MergeComplete merged

stepMerge :: MergeState a -> Ordering -> (Either (a, a) [a], MergeState a)
stepMerge mergeState@(MergeComplete merged) _ = (Right merged, mergeState)
stepMerge (MergeIncomplete ([x],     right, temp)) GT = (Right merged, MergeComplete merged)
    where merged = temp ++ [x] ++ right
stepMerge (MergeIncomplete (x:left', right, temp)) GT 
    = (Left (head left', head right), MergeIncomplete (left', right, temp ++ [x]))
stepMerge (MergeIncomplete (left, right, temp)) LT 
    = stepMerge (MergeIncomplete (right, left, temp)) GT

data LayerState a = LayerComplete [[a]] | LayerIncomplete ([[a]], MergeState a, [[a]])
    deriving (Show, Read)

startNewLayer :: [[a]] -> SortState a
startNewLayer (currL:currR:next) = SortIncomplete $ LayerIncomplete ([], newMerge currL currR, next)
startNewLayer [xs] = SortComplete xs
startNewLayer [] = SortComplete []

stepLayer :: LayerState a -> Ordering -> (Either (a, a) [[a]], LayerState a)
stepLayer layerState@(LayerComplete layer) _ = (Right layer, layerState)
stepLayer (LayerIncomplete (prev, mergeState@(MergeIncomplete _), next)) ord =
    let (comparisonResult, mergeState') = runState (state (`stepMerge` ord)) mergeState
    in case comparisonResult of
        Right merged -> case next of
            nextL:nextR:next' -> 
                (Left (head nextL, head nextR), LayerIncomplete (prev ++ [merged], newMerge nextL nextR, next'))
            rem -> (Right layer, LayerComplete layer) 
                where layer = prev ++ [merged] ++ rem
        Left nextComparison -> (Left nextComparison, LayerIncomplete (prev, mergeState', next))

data SortState a = SortComplete [a] | SortIncomplete (LayerState a)
    deriving (Show, Read)

judge :: Ordering -> State (SortState a) (Either (a, a) [a])
judge ord = state (`stepSort` ord)

newSort :: [a] -> (Either (a, a) [a], SortState a)
newSort xs@(nextL:nextR:_) = (Left (nextL, nextR), startNewLayer $ map singleton xs)
newSort xs = (Right xs, SortComplete xs)

stepSort :: SortState a -> Ordering -> (Either (a, a) [a], SortState a)
stepSort sortState@(SortComplete sorted) _ = (Right sorted, sortState)
stepSort (SortIncomplete layerState) ord =
    let (comparisonResult, layerState') = runState (state (`stepLayer` ord)) layerState
    in case comparisonResult of
        Right [singleRun] -> (Right singleRun, SortComplete singleRun)
        Right nextLayer@(nextL:nextR:_) -> (Left (head nextL, head nextR), startNewLayer nextLayer)
        Left nextComparison -> (Left nextComparison, SortIncomplete layerState')

currentOrdering :: SortState a -> [[a]]
currentOrdering (SortComplete sorted) = [[x] | x <- sorted]
currentOrdering (SortIncomplete (LayerIncomplete (runsPrev, MergeIncomplete (currLeft, currRight, currRun), runsNext))) =
        -- within (runsPrev ++ runsNext ++ [currRun]), group elements by how many other elements are better
    let groupedFromZero = transpose (runsPrev ++ runsNext ++ [currRun])
        -- same thing as above, but for remaining elements in current merge
        groupedFromCurr = replicate (length currRun) [] ++ transpose [currLeft, currRight]
        -- combine groupedFromZero and groupedFromCurr
        combine xs [] = xs
        combine [] ys = ys
        combine (x:xs) (y:ys) = (x ++ y) : combine xs ys
    in combine groupedFromZero groupedFromCurr
currentOrdering (SortIncomplete (LayerIncomplete (runsPrev, MergeComplete currRun, runsNext)))
    = transpose (runsPrev ++ runsNext ++ [currRun])

currentOrderingRanks :: SortState a -> [(Int, [a])]
currentOrderingRanks mergeSort =
    let groups = currentOrdering mergeSort
        ranks = scanl (+) 1 (map length groups)
    in zipWith (\i xs -> (ranks !! i, xs)) [0..] groups

-- estimate remaining comparisons
estimateComparisonsLeftMerge :: MergeState a -> Int
estimateComparisonsLeftMerge (MergeComplete _) = 0
estimateComparisonsLeftMerge (MergeIncomplete (runL, runR, _)) = 2 * min (length runL) (length runR)

estimateComparisonsLeftLayer :: Int -> Int -> Int
estimateComparisonsLeftLayer blockSize listLength
    | listLength < blockSize   = 0
    | listLength < blockSize*2 = (listLength - blockSize)*2
    | otherwise = (blockSize*2) + estimateComparisonsLeftLayer blockSize (listLength - blockSize*2)

recoverLengthBlockSize :: [[a]] -> [[a]] -> MergeState a -> (Int, Int)
recoverLengthBlockSize runsPrev runsNext (MergeIncomplete (mergeL, mergeR, mergeT)) =
    let blockSize = length mergeL + length mergeR + length mergeT
        len = blockSize + sum (map length (runsPrev ++ runsNext))
    in (len, blockSize)

estimateFutureLayers :: (Int, Int) -> Int
estimateFutureLayers (length, blockSize)
    | blockSize > length = 0
    | otherwise          = estimateFutureLayers (length, blockSize * 2) + estimateComparisonsLeftLayer blockSize length

estimateComparisonsLeft :: SortState a -> Int
estimateComparisonsLeft (SortComplete _) = 0
estimateComparisonsLeft (SortIncomplete (LayerIncomplete (runsPrev, merge, runsNext)))
    = estimateFutureLayers (recoverLengthBlockSize runsPrev runsNext merge) + sum (map length runsNext) + estimateComparisonsLeftMerge merge
