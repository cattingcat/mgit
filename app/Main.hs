module Main where

import Lib
import LibGit.TestGitLib
import FFI.TestFfi

main :: IO ()
main = do
--  testStatusEnumSize
  tstRemote
  tstStatus
--  tstRemoteFetch
  tstBranches
  pure ()
