module Main where

import Test
import qualified LibGit.GitLibCommands as T
import qualified Cli.CliParser as Cli
import qualified Test as Tst

main :: IO ()
main = do
  command <- Cli.parseCli
  case command of
    Cli.Branch        -> T.currentBranches
    Cli.Branches      -> T.aggregateBranches
    Cli.Fetch         -> T.fetchAll
    Cli.SetHead       -> T.setHead
    Cli.LookupRef s   -> T.lookupRef s
    Cli.Checkout s    -> T.checkout s
    Cli.Test          -> Tst.runTst
  pure ()
