module LibGit.Wrappers(
  withLibGit,
  withRepo,
  libGitVersion,
  repoDir,

  DeltaStatus(..),
  DeltaInfo(..),
  StatusEntryDeltaInfo(..),
  StatusInfo(..),
  repoStatus,

  lookupRemote,
  remoteUri,
  remoteFetch,

  branches
) where

import Control.Exception (Exception, throw)
import GHC.Generics (Generic)
import Data.Data (Typeable)

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable
import Foreign.Ptr

import qualified LibGit.Common as C
import LibGit.Models
import qualified LibGit.Status as S
import qualified LibGit.Remote as R
import qualified LibGit.Branch as B



newtype OpenRepoError = OpenRepoError CInt deriving (Show, Typeable)
instance Exception OpenRepoError

type GitRepoPtr = Ptr GitRepo

withLibGit :: IO a -> IO a
withLibGit io = do
  C.c_git_libgit2_init
  a <- io
  C.c_git_libgit2_shutdown
  pure a

withRepo :: FilePath -> (GitRepoPtr -> IO a) -> IO a
withRepo path f = do
  p <- mallocBytes (sizeOf (undefined :: Ptr (Ptr GitRepo)))
  r <- withCString path (C.c_git_repository_open p)
  if r /= 0
    then throw (OpenRepoError r)
    else pure ()
  repoPtr <- peek p
  res <- f repoPtr
  free p
  pure res

showVer :: CInt -> CInt -> CInt -> String
showVer mj mn pc = show mj ++ "." ++ show mn ++ "." ++ show pc

libGitVersion :: IO String
libGitVersion = do
  majorPtr <- mallocBytes (sizeOf (undefined :: Ptr CInt))
  minorPtr <- mallocBytes (sizeOf (undefined :: Ptr CInt))
  patchPtr <- mallocBytes (sizeOf (undefined :: Ptr CInt))
  C.c_git_libgit2_version majorPtr minorPtr patchPtr
  verStr <- showVer
    <$> peek majorPtr
    <*> peek minorPtr
    <*> peek patchPtr
  free majorPtr
  free minorPtr
  free patchPtr
  pure verStr

repoDir :: GitRepoPtr -> IO FilePath
repoDir ptr = do
  pathPtr <- C.c_git_repository_commondir ptr
  path <- peekCString pathPtr
  free pathPtr
  pure path

data DeltaStatus =
    Unmodified
  | Added
  | Deleted
  | Modified
  | Renamed
  | Copied
  | Ignored
  | Untracked
  | Typechange
  | Unreadable
  | Conflicted
  deriving (Show)

data DeltaInfo = DeltaInfo {
  status :: DeltaStatus,
  oldPath :: FilePath,
  newPath :: FilePath
} deriving (Show)

data StatusEntryDeltaInfo = StatusEntryDeltaInfo {
  headToIndex :: Maybe DeltaInfo,
  indexToWorkDir :: Maybe DeltaInfo
} deriving (Show)

newtype StatusInfo = StatusInfo {
  delta :: [StatusEntryDeltaInfo]
} deriving (Show)

repoStatus :: GitRepoPtr -> IO StatusInfo
repoStatus ptr = do
  p <- malloc
  S.c_git_status_list_new_integr p ptr
  statusListPtr <- peek p
  count <- S.c_git_status_list_entrycount statusListPtr
  entries <- loop statusListPtr count 0
  deltaInfos <- mapM mapStatusEntry entries
  S.c_git_status_list_free statusListPtr
  free p
  pure (StatusInfo deltaInfos)
  where
    loop :: Ptr S.GitStatusList -> CSize -> CSize -> IO [S.GitStatusEntry]
    loop listPtr count i = if i == count then pure [] else do
      entryPtr <- S.c_git_status_byindex listPtr i
      entry <- peek entryPtr
      tail <- loop listPtr count (i + 1)
      pure (entry : tail)

mapStatusEntry :: S.GitStatusEntry -> IO StatusEntryDeltaInfo
mapStatusEntry (S.GitStatusEntry s h2i i2w) = do
  h2iPaths <- mapDeltaPtr h2i
  i2wPaths <- mapDeltaPtr i2w
  pure $ StatusEntryDeltaInfo h2iPaths i2wPaths

mapDeltaPtr :: Ptr S.GitDiffDelta -> IO (Maybe DeltaInfo)
mapDeltaPtr ptr = if ptr /= nullPtr
  then do
    delta <- peek ptr
    paths <- mapDelta delta
    pure (Just paths)
  else pure Nothing

mapDelta :: S.GitDiffDelta -> IO DeltaInfo
mapDelta (S.GitDiffDelta s _ _ _ (S.GitDiffFile _ p1 _ _ _ _) (S.GitDiffFile _ p2 _ _ _ _)) = do
  oldPath <- peekCString p1
  newPath <- peekCString p2
  pure $ DeltaInfo (mapStatus s) oldPath newPath


mapStatus :: CUInt -> DeltaStatus
mapStatus 0  = Unmodified
mapStatus 1  = Added
mapStatus 2  = Deleted
mapStatus 3  = Modified
mapStatus 4  = Renamed
mapStatus 5  = Copied
mapStatus 6  = Ignored
mapStatus 7  = Untracked
mapStatus 8  = Typechange
mapStatus 9  = Unreadable
mapStatus 10 = Conflicted


type GitRemotePtr = Ptr R.GitRemote

lookupRemote :: GitRepoPtr -> String -> (GitRemotePtr -> IO a) -> IO a
lookupRemote repo name f = do
  p <- malloc
  r <- withCString name (R.c_git_remote_lookup p repo)
  remotePtr <- peek p
  res <- f remotePtr
  R.c_git_remote_free remotePtr
  free p
  pure res
  
  
remoteUri :: GitRemotePtr -> IO String
remoteUri remote = do 
  uriStr <- R.c_git_remote_url remote
  peekCString uriStr

remoteFetch :: GitRemotePtr -> IO ()
remoteFetch remote = do
  optsPtrPtr <- malloc
  R.c_git_fetch_init_options_integr optsPtrPtr
  optsPtr <- peek optsPtrPtr
  r <- R.c_git_remote_fetch remote nullPtr optsPtr nullPtr
  free optsPtr
  free optsPtrPtr
  print r
  pure ()


data BranchType = RemoteBranch | LocalBranch
  deriving (Show)

data BranchInfo = BranchInfo {
  branchType :: BranchType,
  name :: String
} deriving (Show)

branches :: GitRepoPtr -> IO [BranchInfo]
branches repoPtr = do
  iterPtrPtr <- malloc
  strPtr <- malloc
  refPtrPtr <- malloc
  branchTypePtr <- malloc

  B.c_git_branch_iterator_new iterPtrPtr repoPtr B.allBranches
  iterPtr <- peek iterPtrPtr
  res <- loop refPtrPtr branchTypePtr iterPtr strPtr []

  free strPtr
  free refPtrPtr
  free branchTypePtr

  B.c_git_branch_iterator_free iterPtr
  free iterPtrPtr

  print res

  pure res

  where
    loop rpp branchTypePtr iterPtr branchNamePtr accum = do
      nextRes <- B.c_git_branch_next rpp branchTypePtr iterPtr
      refPtr <- peek rpp
      B.c_git_branch_name branchNamePtr refPtr
      str <- peek branchNamePtr
      branchName <- peekCString str
      branchType <- peek branchTypePtr
      let
        bType = if branchType == B.localBranch
          then LocalBranch
          else RemoteBranch
        r = BranchInfo bType branchName
        rs = r:accum
      if nextRes == 0
        then loop rpp branchTypePtr iterPtr branchNamePtr rs
        else pure rs