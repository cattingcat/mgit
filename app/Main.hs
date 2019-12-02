module Main where

import Lib
import FFI.LibGit
import CFilesTest

main :: IO ()
main = do
  tstRepoOpen
  tstLibGitVersion
  tstCFiles
  pure ()
