module Main where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Misc.Sort
import Control.Monad.Trans.State (runState)

interactiveSort :: [Int] -> [Int]
interactiveSort xs =
    let state0 = newSort xs
        comparator (Left (valueLeft, valueRight)) = compare valueLeft valueRight
        comparator (Right _) = error "impossible"
        state' = until (\(a, s) -> case a of 
            Left _ -> False
            Right _ -> True) 
            (\(a, s) -> runState (judge (comparator a)) s) state0
    in case state' of
        (_, SortComplete sorted) -> sorted
        (_, SortIncomplete _) -> error "impossible"

sortTest1 :: Assertion
sortTest1 = assertEqual "should be sorted" [8,7..1] (interactiveSort [8,1,7,2,6,3,5,4])

sortTest2 :: Assertion
sortTest2 = assertEqual "should be sorted" [9,8..1] (interactiveSort [1,7,2,6,9,3,5,8,4])

sortTest3 :: Assertion
sortTest3 = assertEqual "should be sorted" [] (interactiveSort [])

sortTest4 :: Assertion
sortTest4 = assertEqual "should be sorted" [1] (interactiveSort [1])

main :: IO ()
main = defaultMainWithOpts
    [
        testCase "sort even list" sortTest1,
        testCase "sort odd list" sortTest2,
        testCase "sort empty list" sortTest3,
        testCase "sort singleton list" sortTest4
    ]
    mempty
