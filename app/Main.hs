module Main where

import Lib
import LibGit.Test
import CFilesTest

main :: IO ()
main = do
  tstRepoOpen
  tstLibGitVersion
  tstCFiles
  tstRepoStatus
  pure ()
