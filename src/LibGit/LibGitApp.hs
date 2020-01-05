module LibGit.LibGitApp (
  LibGitApp,
  runLibGitApp,
  runLibGitApps
) where

import Control.Monad.State

import System.Directory

import MGit.MonadGit
import MGit.StatusModels
import MGit.BranchModels

import qualified LibGit.Models as M
import qualified LibGit.Remote as R
import qualified LibGit.Repository as Re
import qualified LibGit.Status as S
import qualified LibGit.Common as C
import qualified LibGit.Branch as B
import Data.Maybe (isJust)


data LibGitAppState = LibGitAppState {
  repoPtr :: M.GitRepoPtr,
  originRemotePtr :: R.GitRemotePtr
}

type LibGitApp = StateT LibGitAppState IO

instance MonadGit LibGitApp where
  fetch = do
    remote <- gets originRemotePtr
    fetchRes <- lift $ R.remoteFetch remote
    lift $ print "fetched"
    pure ()

  branches = do
    repo <- gets repoPtr
    lift $ B.getBranches repo

  status = do
    repo <- gets repoPtr
    lift $ S.repoStatus repo

  path = do
    repo <- gets repoPtr
    lift $ Re.repoDir repo

  setHead (RefName name) = do
    repo <- gets repoPtr
    lift $ Re.setHead repo name


runLibGitApp :: FilePath -> LibGitApp a -> IO a
runLibGitApp path m = C.withLibGit $
  C.withRepo path $ \repo ->
    R.lookupRemote repo "origin" $ \remote -> do
      (a, s) <- runStateT m (LibGitAppState repo remote)
      pure a

runLibGitApps :: forall a . [FilePath] -> LibGitApp a -> IO [a]
runLibGitApps paths app = C.withLibGit $ filterNothings ioAs
  where
    filterNothings :: IO [Maybe a] -> IO [a]
    filterNothings io = do
      mas <- io
      pure $ do
        a <- mas
        case a of
          Just r -> [r]
          _      -> []

    ioAs :: IO [Maybe a]
    ioAs = mapM foo paths

    foo :: FilePath -> IO (Maybe a)
    foo path = C.withRepoSafe path $ \case
      Just repo -> Just <$> processRepo repo
      Nothing -> pure Nothing

    processRepo repo = R.lookupRemote repo "origin" $ \remote -> do
      (a, s) <- runStateT app (LibGitAppState repo remote)
      pure a
