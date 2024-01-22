module Main where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Misc.Util

sortTest :: Assertion
sortTest = print "Sort Test"

main :: IO ()
main = defaultMainWithOpts
       [testCase "sortTest" sortTest]
       mempty
