{-# OPTIONS_GHC -Wno-unused-matches #-}
module Misc.Sort where

import qualified System.Random
import Data.Array.IO hiding (newArray)
import Control.Monad
import Data.List (transpose)
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
    deriving Show

comparisonToMergeState :: Ordering -> State (MergeState a) (Either (a, a) [a])
comparisonToMergeState ord = state (\mergeState ->
    let mergeState' = stepMerge mergeState ord
    in case mergeState' of
        MergeComplete merged -> (Right merged, mergeState')
        MergeIncomplete (left, right, _) -> (Left (head left, head right), mergeState')
    )

stepMerge :: MergeState a -> Ordering -> MergeState a
stepMerge mergeState@(MergeComplete mergeRun) _ = mergeState
stepMerge ms EQ = ms
stepMerge (MergeIncomplete (left, right, temp)) GT =
    let temp' = temp ++ take 1 left
        left' = drop 1 left
    in case left' of
        [] -> MergeComplete (temp' ++ right)
        _  -> MergeIncomplete (left', right, temp')
stepMerge (MergeIncomplete (left, right, temp)) LT
    = stepMerge (MergeIncomplete (right, left, temp)) GT

data MergeLayerState a = MergeLayerComplete [[a]] | MergeLayerIncomplete ([[a]], MergeState a, [[a]])
    deriving Show

comparisonToMergeLayerState :: Ordering -> State (MergeLayerState a) (Either (a, a) [[a]])
comparisonToMergeLayerState ord = state (`stepMergeLayer` ord)

stepMergeLayer :: MergeLayerState a -> Ordering -> (Either (a, a) [[a]], MergeLayerState a)
stepMergeLayer layerState@(MergeLayerComplete layer) _ = (Right layer, layerState)
stepMergeLayer (MergeLayerIncomplete (prev, mergeState@(MergeIncomplete _), next)) ord =
    let (comparisonResult, mergeState') = runState (comparisonToMergeState ord) mergeState
    in case comparisonResult of
        Right merged ->
            let prev' = prev ++ filter (not . null) [merged]
            in case (prev', next) of
                (_, [])          -> (Right prev',  MergeLayerComplete prev')
                (_, [singleRun]) -> (Right prev'', MergeLayerComplete prev'')
                    where prev'' = prev' ++ [singleRun]
                (_, nextL:nextR:next') -> (Left (head nextL, head nextR), MergeLayerIncomplete (prev', MergeIncomplete (nextL, nextR, []), next'))
        Left nextComparison -> (Left nextComparison, MergeLayerIncomplete (prev, mergeState', next))
stepMergeLayer (MergeLayerIncomplete (_, MergeComplete _, _)) _ = error "Not allowed 1"

data MergeSortState a = MergeSortComplete [a] | MergeSortIncomplete (MergeLayerState a)
    deriving Show

comparisonToMergeSortState :: Ordering -> State (MergeSortState a) (Either (a, a) [a])
comparisonToMergeSortState ord = state (`stepMergeSort` ord)

initialMergeSortState :: [a] -> (Either (a, a) [a], MergeSortState a)
initialMergeSortState [ ] = (Right [ ], MergeSortComplete [ ])
initialMergeSortState [x] = (Right [x], MergeSortComplete [x])
initialMergeSortState (nextL:nextR:remaining) = (Left (nextL, nextR), MergeSortIncomplete (MergeLayerIncomplete ([], MergeIncomplete ([nextL], [nextR], []), [[x] | x <- remaining])))

stepMergeSort :: MergeSortState a -> Ordering -> (Either (a, a) [a], MergeSortState a)
stepMergeSort sortState@(MergeSortComplete sorted) _ = (Right sorted, sortState)
stepMergeSort (MergeSortIncomplete layerState) ord =
    let (comparisonResult, layerState') = runState (comparisonToMergeLayerState ord) layerState
    in case comparisonResult of
        Right []          -> (Right [], MergeSortComplete [])
        Right [singleRun] -> (Right singleRun, MergeSortComplete singleRun)
        Right (nextL:nextR:nextLayer) -> (Left (head nextL, head nextR), MergeSortIncomplete (MergeLayerIncomplete ([], MergeIncomplete (nextL, nextR, []), nextLayer)))
        Left nextComparison -> (Left nextComparison, MergeSortIncomplete layerState')

currentOrdering :: MergeSortState a -> [[a]]
currentOrdering (MergeSortComplete sorted) = [[x] | x <- sorted]
currentOrdering (MergeSortIncomplete (MergeLayerIncomplete (runsPrev, MergeIncomplete (currLeft, currRight, currRun), runsNext))) =
        -- within (runsPrev ++ runsNext ++ [currRun]), group elements by how many other elements are better
    let groupedFromZero = transpose (runsPrev ++ runsNext ++ [currRun])
        -- same thing as above, but for remaining elements in current merge
        groupedFromCurr = replicate (length currRun) [] ++ transpose [currLeft, currRight]
        -- combine groupedFromZero and groupedFromCurr
        combine :: [[a]] -> [[a]] -> [[a]]
        combine xs [] = xs
        combine [] ys = ys
        combine (x:xs) (y:ys) = (x ++ y) : combine xs ys
    in combine groupedFromZero groupedFromCurr
currentOrdering (MergeSortIncomplete (MergeLayerIncomplete (runsPrev, MergeComplete currRun, runsNext)))
    = transpose (runsPrev ++ runsNext ++ [currRun])

currentOrderingRanks :: MergeSortState a -> [(Int, [a])]
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

estimateComparisonsLeft :: MergeSortState a -> Int
estimateComparisonsLeft (MergeSortComplete _) = 0
estimateComparisonsLeft (MergeSortIncomplete (MergeLayerIncomplete (runsPrev, merge, runsNext)))
    = estimateFutureLayers (recoverLengthBlockSize runsPrev runsNext merge) + sum (map length runsNext) + estimateComparisonsLeftMerge merge
