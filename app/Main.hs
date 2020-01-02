module Main where

import Lib
import qualified LibGit.TestGitLib as T
import Foreign.TestFfi

main :: IO ()
main = do
  print "MGit app"
--  testStatusEnumSize
--  tstRemote
--  tstStatus
--  tstRemoteFetch
--  tstBranches
  T.tstCurrentBranches
  pure ()
