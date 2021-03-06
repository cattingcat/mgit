module LibGit.GitApp (
  LibGitApp,
  runLibGitApp,
  runLibGitApps
) where

import System.IO (IO)
import System.FilePath

import Data.Maybe
import Data.Bool
import Data.Text qualified as T
import Data.Function (($))
import Control.Monad
import Control.Applicative
import Control.Monad.State

import MGit.MonadGit
import MGit.RefModels

import Foreign.LibGit.Models qualified as M
import Foreign.LibGit.Checkout qualified as Chk

import LibGit.Remote qualified as R
import LibGit.Repository qualified as Re
import LibGit.Refs qualified as Ref
import LibGit.AnnotatedCommit qualified as A
import LibGit.Commit qualified as Comm
import LibGit.Status qualified as S
import LibGit.Common qualified as C
import LibGit.Branch qualified as B
import GHC.Err (error)
import Data.List (last)


data LibGitAppState = LibGitAppState {
  repoPtr :: M.GitRepoPtr,
  originRemotePtr :: R.GitRemotePtr
}

type LibGitApp = StateT LibGitAppState IO

instance MonadGit LibGitApp where
  fetch = do
    remote <- gets originRemotePtr
    _ <- lift $ R.remoteFetch remote
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

  lookupRef str = do
    repo <- gets repoPtr
    lookupRes <- lift $ Ref.lookupRef repo str
    case lookupRes of
      Nothing  -> pure Nothing
      Just ref -> lift $ do
        refName <- Ref.refName ref
        refType <- getRefType ref
        annotComm <- A.getAnnotatedCommit repo ref
        commId <- A.commitId annotComm
        commit <- Comm.getCommit repo commId
        message <- Comm.commitMessage commit

        Comm.freeCommit commit
        A.freeAnnotatedCommit annotComm
        Ref.freeRef ref
        pure $ Just (RefInfo refType (RefName refName) message)

  checkoutTree (RefName refName) = do
    repo <- gets repoPtr
    lookupRes <- lift $ Ref.lookupRef repo refName
    case lookupRes of
      Nothing  -> error "fatal"
      -- todo: ^ fix
      Just ref -> lift $ do
        annotComm <- A.getAnnotatedCommit repo ref
        commId <- A.commitId annotComm
        commit <- Comm.getCommit repo commId
        _ <- Chk.c_git_checkout_tree_integr repo commit
        refType <- getRefType ref
        case refType of
          Remote -> do
            let newBranchName = last $ T.splitOn "/" refName
            newRef <- B.createBranchFromRemote repo newBranchName annotComm
            newRefName <- Ref.refName newRef
            Re.setHead repo newRefName
          Head -> do
            annotCommName <- A.annotatedCommitName annotComm
            Re.setHead repo annotCommName
          Tag -> pure ()

        A.freeAnnotatedCommit annotComm
        Comm.freeCommit commit


runLibGitApp :: FilePath -> LibGitApp a -> IO a
runLibGitApp repoPath m = C.withLibGit $
  C.withRepo repoPath $ \repo ->
    R.lookupRemote repo "origin" $ \remote -> do
      (a, _) <- runStateT m (LibGitAppState repo remote)
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
    foo repoPath = C.withRepoSafe repoPath $ \case
      Just repo -> Just <$> processRepo repo
      Nothing -> pure Nothing

    processRepo repo = R.lookupRemote repo "origin" $ \remote -> do
      (a, _) <- runStateT app (LibGitAppState repo remote)
      pure a


getRefType :: M.GitRefPtr -> IO RefType
getRefType ref = do
  isLocal <- Ref.isBranch ref
  isRemote <- Ref.isRemote ref
  isTag <- Ref.isTag ref
  pure $ case (isLocal, isRemote, isTag) of
    (True, _, _)          -> Head
    (False, True, _)      -> Remote
    (False, False, True)  -> Tag
    _                     -> error "unknown flag configuration"
