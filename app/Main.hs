module Main where

import System.IO (IO)

import qualified LibGit.GitLibCommands as T
import qualified Cli.CliParser as Cli
import qualified Test as Tst

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
