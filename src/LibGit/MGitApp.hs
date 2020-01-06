module LibGit.MGitApp (
  MGitApp,
  runMGitApp
) where

import Debug.Trace (trace)

import Control.Monad.Reader

import System.Directory as Dir

import LibGit.LibGitApp

import MGit.MonadMGit
import qualified MGit.MonadGit as MG
import qualified MGit.StatusModels as S
import qualified MGit.BranchModels as B



newtype MGitAppState = MGitAppState {
  rootPath :: FilePath
}

type MGitApp = ReaderT MGitAppState IO

instance MonadMGit MGitApp where
  fetch = do
    path <- asks rootPath
    directories <- lift $ Dir.listDirectory path
    lift $ fetchAll directories
    pure ()
    where
      fetchAll dirs = runLibGitApps dirs MG.fetch

  currentBranches = do
    path <- asks rootPath
    directories <- lift $ Dir.listDirectory path
    infos <- lift $ loadAll directories
    pure $ BranchesInfo infos
    where
      loadAll dirs = runLibGitApps dirs loadRepoInfo
      loadRepoInfo = liftM2 RepoBranchInfo MG.path MG.currentBranch

  branchesAll = do
    path <- asks rootPath
    directories <- lift $ Dir.listDirectory path
    lift $ runLibGitApps directories $ do
      path <- MG.path
      branches <- MG.branches
      pure (path, branches)


runMGitApp :: FilePath -> MGitApp a -> IO a
runMGitApp pwd app = runReaderT app $ MGitAppState pwd
