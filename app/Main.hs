module Main where

import Lib
import LibGit.TestGitLib
import CFilesTest
import FFI.TestFfi

main :: IO ()
main = do
--  testStatusEnumSize
  tstRemoteNew
  tstStatusNew
--  tstRepoOpen
--  tstLibGitVersion
--  tstCFiles
--  tstRepoStatus
--  tstRepoOrigin
--  tstFetch2
  pure ()
