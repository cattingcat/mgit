module LibGit.MGitApp (
  MGitApp,
  runMGitApp
) where

import Control.Applicative 
import Control.Monad.Reader
import Control.Category ((.))
import Data.Functor (($>))
import Data.Function (($))

import System.IO (IO)
import System.FilePath
import System.Directory as Dir

import LibGit.GitApp

import MGit.MonadMGit
import MGit.MonadMassAction
import MGit.MonadGit qualified as MG
import MGit.RefModels qualified as R


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
  fetch = mrun MG.fetch $> ()

  currentBranches = BranchesInfo <$> mrun (RepoBranchInfo <$> MG.path <*> MG.currentBranch)

  branchesAll = mrun $ (,) <$> MG.path <*> MG.branches

  checkout (CheckoutSpec list) = lift $ mapM changeBranch list $> ()
    where
      changeBranch :: (FilePath, R.RefName) -> IO ()
      changeBranch (path, ref) = runLibGitApp path (MG.checkoutTree ref)

  checkoutSafe search = mrun (MG.checkoutSafe search) $> ()


runMGitApp :: FilePath -> MGitApp a -> IO a
runMGitApp pwd app = runReaderT app $ MGitAppState pwd
