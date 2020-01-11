module LibGit.GitLibCommands (
  currentBranches,
  aggregateBranches,
  fetchAll,
  lookupBranches,
  checkout,
  repos
) where

import System.IO (IO)
import System.Directory
import Control.Applicative
import Data.Eq
import Data.Text
import Data.Text.IO

import qualified LibGit.MGitApp as MA
import qualified MGit.MonadMGit as MA
import qualified MGit.Format as F


currentBranches :: IO ()
currentBranches = do
  pwd <- getCurrentDirectory
  res <- MA.runMGitApp pwd MA.currentBranches
  F.printBranchesInfo pwd res
  pure ()

aggregateBranches :: IO ()
aggregateBranches = do
  pwd <- getCurrentDirectory
  res <- MA.runMGitApp pwd (MA.aggregateRemoteBranchCount (==))
  F.printBranchAggregationInfo res

fetchAll :: IO ()
fetchAll = do
  pwd <- getCurrentDirectory
  _ <- MA.runMGitApp pwd MA.fetch
  putStrLn "fetched all"

lookupBranches :: Text -> IO ()
lookupBranches s = do
  pwd <- getCurrentDirectory
  res <- MA.runMGitApp pwd (MA.lookupBranches s)
  F.printBranchesLookup pwd res

checkout :: Text -> IO ()
checkout s = do
  pwd <- getCurrentDirectory
  MA.runMGitApp pwd (MA.checkoutSafe s)
  putStrLn "Checkout done"

repos :: IO ()
repos = do
  pwd <- getCurrentDirectory
  rs <- MA.runMGitApp pwd MA.repos
  F.printRepos rs 
