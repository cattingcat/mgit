module Main where

import Lib
import qualified LibGit.GitLibCommands as T
import qualified Cli.CliParser as Cli
import Foreign.TestFfi

main :: IO ()
main = do
  command <- Cli.parseCli
  case command of
    Cli.Branch        -> T.currentBranches
    Cli.Branches      -> T.aggregateBranches
    Cli.Fetch         -> T.fetchAll
    Cli.Test          -> testStatusEnumSize
  pure ()
