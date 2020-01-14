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
import MGit.MonadMassAction
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
      
instance MonadMassAction LibGitApp MGitApp where 
  mrun m = do
    rs <- repos
    lift $ runLibGitApps rs m 


instance MonadMGit MGitApp where
  fetch = do
    mrun MG.fetch
    pure ()

  currentBranches = do
    infos <- mrun loadRepoInfo
    pure $ BranchesInfo infos
    where
      loadRepoInfo = liftM2 RepoBranchInfo MG.path MG.currentBranch

  branchesAll = mrun $ do
      repoPath <- MG.path
      branches <- MG.branches
      pure (repoPath, branches)

  checkout (CheckoutSpec list) = do
    -- todo: ^ simplify interface, use only branch-name 
    lift $ mapM changeBranch list
    pure ()
      where
        changeBranch :: (FilePath, R.RefName) -> IO ()
        changeBranch (path, ref) = runLibGitApp path (MG.checkoutTree ref)
          

runMGitApp :: FilePath -> MGitApp a -> IO a
runMGitApp pwd app = runReaderT app $ MGitAppState pwd
