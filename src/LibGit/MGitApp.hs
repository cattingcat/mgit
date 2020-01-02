{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module LibGit.MGitApp (
  MGitApp,
  runMGitApp
) where

import Control.Monad.Reader

import System.Directory as Dir

import LibGit.LibGitApp

import MGit.MonadMGit
import qualified MGit.MonadGit as MG
import qualified MGit.StatusModels as S
import qualified MGit.BranchModels as B
import Debug.Trace (trace)


data MGitAppState = MGitAppState {
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

  getBranches = do
    path <- asks rootPath
    directories <- lift $ Dir.listDirectory path
    infos <- lift $ loadAll directories
    pure $ BranchesInfo infos
    where
      loadAll dirs = runLibGitApps dirs loadRepoInfo
      loadRepoInfo = do
        p <- MG.path
        bs <- MG.currentBranch
        pure $ BranchInfo p bs



runMGitApp :: MGitApp a -> IO a
runMGitApp app = do
  pwd <- getCurrentDirectory
  runReaderT app $ MGitAppState pwd