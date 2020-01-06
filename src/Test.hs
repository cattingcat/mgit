module Test (
  runTst
) where

import Test.TestExts
import Test.TestFfi

runTst :: IO ()
runTst = do
  tstFfi
  tstExts
