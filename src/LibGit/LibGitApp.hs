{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module LibGit.LibGitApp where

import Control.Monad.State

import System.Directory

import MGit.MonadGit
import MGit.StatusModels
import MGit.BranchModels

import qualified LibGit.Models as M
import qualified LibGit.Remote as R
import qualified LibGit.Status as S
import qualified LibGit.Common as C
import qualified LibGit.Branch as B


data LibGitAppState = LibGitAppState {
  repoPtr :: M.GitRepoPtr,
  originRemotePtr :: R.GitRemotePtr
}

type LibGitApp = StateT LibGitAppState IO

instance MonadGit LibGitApp where
  fetch = do
    remote <- gets originRemotePtr
    fetchRes <- lift $ R.remoteFetch remote
    pure ()

  branches = do
    repo <- gets repoPtr
    branchesRes <- lift $ B.getBranches repo
    pure branchesRes

  status = do
    repo <- gets repoPtr
    statusRes <- lift $ S.repoStatus repo
    pure statusRes


runLibGitApp :: FilePath -> LibGitApp a -> IO a
runLibGitApp path m = C.withLibGit $ 
  C.withRepo path $ \repo ->
    R.lookupRemote repo "origin" $ \remote -> do
      (a, s) <- runStateT m (LibGitAppState repo remote)
      pure a
