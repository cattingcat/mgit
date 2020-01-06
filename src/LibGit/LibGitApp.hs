module LibGit.LibGitApp (
  LibGitApp,
  runLibGitApp,
  runLibGitApps
) where

import Data.List.Split
import Data.Maybe (isJust)
import Control.Monad.State

import System.Directory

import MGit.MonadGit
import MGit.StatusModels
import MGit.BranchModels
import MGit.RefModels

import qualified LibGit.Models as M
import qualified LibGit.Remote as R
import qualified LibGit.Repository as Re
import qualified LibGit.Refs as Ref
import qualified LibGit.AnnotatedCommit as A
import qualified LibGit.Commit as Comm
import qualified LibGit.Checkout as Chk
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
    lift $ B.getBranches repo

  status = do
    repo <- gets repoPtr
    lift $ S.repoStatus repo

  path = do
    repo <- gets repoPtr
    lift $ Re.repoDir repo

  lookupRef str = do
    repo <- gets repoPtr
    ref <- lift $ Ref.lookupRef repo str
    case ref of
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
    ref <- lift $ Ref.lookupRef repo refName
    case ref of
      Nothing  -> error "fatal"
      -- todo: ^ fix
      Just ref -> lift $ do
        annotComm <- A.getAnnotatedCommit repo ref
        commId <- A.commitId annotComm
        commit <- Comm.getCommit repo commId
        r <- Chk.c_git_checkout_tree_integr repo commit
        refType <- getRefType ref
        case refType of
          Remote -> do
            let newBranchName = last $ splitOn "/" refName
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


getRefType :: M.GitRefPtr -> IO RefType
getRefType ref = do
   isLocal <- Ref.isBranch ref
   isRemote <- Ref.isRemote ref
   isTag <- Ref.isTag ref
   pure $ case (isLocal, isRemote, isTag) of
    (True, _, _)          -> Head
    (False, True, _)      -> Remote
    (False, False, True)  -> Tag
