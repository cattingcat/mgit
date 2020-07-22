module Main (main) where

import System.IO (IO)

import LibGit.GitLibCommands  qualified as T
import Cli.CliParser          qualified as Cli
import Test                   qualified as Tst

main :: IO ()
main = do
  cmd <- Cli.parseCli
  case cmd of
    Cli.Branch            -> T.currentBranches
    Cli.Branches          -> T.aggregateBranches
    Cli.Fetch             -> T.fetchAll
    Cli.LookupBranches s  -> T.lookupBranches s
    Cli.Checkout s        -> T.checkout s
    Cli.Repos             -> T.repos
    Cli.Test              -> Tst.runTst
