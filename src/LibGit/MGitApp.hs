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


data MGitAppState = MGitAppState {
  rootPath :: FilePath
}

type MGitApp = ReaderT MGitAppState IO

instance MonadMGit MGitApp where
  getBranches = do 
    path <- asks rootPath
    directories <- lift $ Dir.listDirectory path
    infos <- lift $ loadAll directories
    
    lift $ print infos
    
    pure $ BranchesInfo infos
    
    where 
      loadRepoInfo = do
        _ <- MG.fetch
        p <- MG.path
        bs <- MG.branches
        pure $ BranchInfo p (B.name . B.currentBranch $ bs)
          
      loadAll dirs = runLibGitApps dirs loadRepoInfo
      
      
runMGitApp :: MGitApp a -> IO a
runMGitApp app = do
  pwd <- getCurrentDirectory 
  runReaderT app $ MGitAppState pwd 