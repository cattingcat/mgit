module Main where

import Lib
import qualified LibGit.TestGitLib as T
import Foreign.TestFfi

main :: IO ()
main = do
--  testStatusEnumSize
--  tstRemote
--  tstStatus
--  tstRemoteFetch
--  tstBranches
  T.tstCurrentBranches
  pure ()
