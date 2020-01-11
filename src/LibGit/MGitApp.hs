module LibGit.MGitApp (
  MGitApp,
  runMGitApp
) where

import Control.Applicative 
import Control.Monad.Reader
import Control.Category ((.))
import Data.Function (($))

import System.IO (IO)
import System.FilePath
import System.Directory as Dir

import LibGit.LibGitApp

import MGit.MonadMGit
import qualified MGit.MonadGit as MG
import qualified MGit.RefModels as R


newtype MGitAppState = MGitAppState {
  rootPath :: FilePath
}

type MGitApp = ReaderT MGitAppState IO

instance MonadMultiRepo MGitApp where
  repos = do
    path <- asks rootPath
    lift $ do
      subdirs <- Dir.listDirectory path
      filterM (Dir.doesPathExist . (</> ".git/")) subdirs

instance MonadMGit MGitApp where
  fetch = do
    rs <- repos
    lift $ fetchAll rs
    pure ()
    where
      fetchAll dirs = runLibGitApps dirs MG.fetch

  currentBranches = do
    rs <- repos
    infos <- lift $ loadAll rs
    pure $ BranchesInfo infos
    where
      loadAll dirs = runLibGitApps dirs loadRepoInfo
      loadRepoInfo = liftM2 RepoBranchInfo MG.path MG.currentBranch

  branchesAll = do
    rs <- repos
    lift $ runLibGitApps rs $ do
      repoPath <- MG.path
      branches <- MG.branches
      pure (repoPath, branches)

  checkout (CheckoutSpec list) = do
    lift $ mapM changeBranch list
    pure ()
      where
        changeBranch :: (FilePath, R.RefName) -> IO ()
        changeBranch (path, ref) = runLibGitApp path (MG.checkoutTree ref)
          

runMGitApp :: FilePath -> MGitApp a -> IO a
runMGitApp pwd app = runReaderT app $ MGitAppState pwd
