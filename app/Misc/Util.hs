{-# OPTIONS_GHC -Wno-unused-matches #-}
module Misc.Util where

import System.Random
import Data.Array.IO hiding (newArray)
import Control.Monad
import Data.IntMap

-- https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
    where
        n = length xs
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray k = newListArray (1, k)

type Range = (Int, Int) -- (start, length)

data MergeSortComputation = Sort Range | Merge Range Range
    deriving (Show)

expand :: MergeSortComputation -> [MergeSortComputation]
expand m@(Merge _ _) = [m]
expand   (Sort (_, 1)) = []
expand   (Sort (start, len)) =
    let mid = div len 2
        rangeL = (start, mid)
        rangeR = (start + mid, len - mid)
    in concat [expand (Sort rangeL), expand (Sort rangeR), [Merge rangeL rangeR]]

type MergeSortHelper a = (IntMap a, Maybe (MergeHelper a), [MergeSortComputation])
    -- (source list, steps, next computations)

mergeSortHelper :: IntMap a -> [MergeSortComputation] -> MergeSortHelper a
mergeSortHelper src [] = (src, Nothing, [])
mergeSortHelper src (comp:comps) = (src, Just (mergeHelper src comp), comps)

mergeSortHelper' :: [a] -> MergeSortHelper a
mergeSortHelper' src = mergeSortHelper (fromList (zip [0..] src)) (expand (Sort (0, length src)))

nextComputation :: MergeSortHelper a -> Maybe (a, a)
nextComputation (_, Nothing, _) = Nothing
nextComputation (_, Just currentMerge, _) = Just (getCandidates currentMerge)

updateMergeSort :: Ordering -> MergeSortHelper a -> MergeSortHelper a
updateMergeSort _ msh@(_, Nothing, _) = msh
updateMergeSort ord (src, Just currentMerge, comps) =
    let currentMerge' = updateMerge currentMerge ord
        (doneL, doneR) = isMergeDetermined currentMerge'
    in if doneL
        then mergeSortHelper (mergeResult $ completeMergeRight currentMerge') comps
    else if doneR
        then mergeSortHelper (mergeResult $ completeMergeLeft currentMerge') comps
    else (src, Just currentMerge', comps)

isMergeSortComplete :: MergeSortHelper a -> Bool
isMergeSortComplete (_, Nothing, _) = True
isMergeSortComplete _ = False

mergeSortResult :: MergeSortHelper a -> [a]
mergeSortResult (src, _, _) = elems src

type MergeHelper a = (IntMap a, IntMap a, Range, Range, Int, Int, Int)
    -- (source list, temp list, range left, range right, index left, index right, index temp)

mergeHelper :: IntMap a -> MergeSortComputation -> MergeHelper a
mergeHelper src (Merge rangeL@(startL, _) rangeR@(startR, _)) = (src, empty, rangeL, rangeR, startL, startR, startL)
mergeHelper src _ = error "Cannot call mergeHelper on Sort computation (use the expand function)"

getCandidates :: MergeHelper a -> (a, a)
getCandidates (src, _, _, _, indexL, indexR, _) = (src ! indexL, src ! indexR)

updateMerge :: MergeHelper a -> Ordering -> MergeHelper a
updateMerge (src, temp, rangeL, rangeR, indexL, indexR, indexT) GT =
    let temp' = insert indexT (src ! indexL) temp
    in (src, temp', rangeL, rangeR, indexL + 1, indexR, indexT + 1)
updateMerge (src, temp, rangeL, rangeR, indexL, indexR, indexT) LT =
    let temp' = insert indexT (src ! indexR) temp
    in (src, temp', rangeL, rangeR, indexL, indexR + 1, indexT + 1)
-- TODO add equality
updateMerge _ _ = mergeHelper (fromList []) (Merge (0, 0) (0, 0))

isMergeDetermined :: MergeHelper a -> (Bool, Bool)
isMergeDetermined (_, _, (startL, endL), (startR, endR), indexL, indexR, _) 
    = (indexL >= startL + endL, indexR >= startR + endR)

completeMergeLeft :: MergeHelper a -> MergeHelper a
completeMergeLeft mh@(src, temp, rangeL, rangeR, indexL, indexR, indexT) =
    if isMergeComplete mh
        then mh
    else let temp' = insert indexT (src ! indexL) temp
         in completeMergeLeft (src, temp', rangeL, rangeR, indexL + 1, indexR, indexT + 1)

completeMergeRight :: MergeHelper a -> MergeHelper a
completeMergeRight mh@(src, temp, rangeL, rangeR, indexL, indexR, indexT) =
    if isMergeComplete mh
        then mh
    else let temp' = insert indexT (src ! indexR) temp
         in completeMergeRight (src, temp', rangeL, rangeR, indexL, indexR + 1, indexT + 1)

isMergeComplete :: MergeHelper a -> Bool
isMergeComplete mh =
    let (doneLeft, doneRight) = isMergeDetermined mh
    in doneLeft && doneRight

mergeResult :: MergeHelper a -> IntMap a
mergeResult (src, temp, _, _, _, _, _) = mapWithKey (\k v -> if member k temp then temp ! k else v) src
