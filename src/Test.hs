module Test (
  runTst
) where

import System.IO (IO)
import Test.TestExts
import Test.TestFfi

runTst :: IO ()
runTst = do
  tstFfi
  tstExts
