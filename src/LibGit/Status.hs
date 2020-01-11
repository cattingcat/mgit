{-# LANGUAGE ForeignFunctionInterface #-}

module LibGit.Status (
  DeltaStatus(..),
  DeltaInfo(..),
  StatusEntryDeltaInfo(..),
  StatusInfo(..),
  repoStatus,
  
  sizeOfGitDiffFile,
  sizeOfGitDiffDelta
) where

import System.IO (IO)

import Control.Monad
import Control.Applicative

import Text.Show
import Data.Maybe
import Data.Eq
import Data.Function (($))

import GHC.Err (error)
import GHC.Base (undefined)
import GHC.Num
import GHC.Generics (Generic)

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable
import Foreign.CVector
import Foreign.CStorableWrap

import LibGit.Models
import MGit.StatusModels


-- | Status Foreign DTOs

data GitStatusList = GitStatusList
  deriving stock (Generic)
  deriving anyclass (CStorable)

data GitDiffFile = GitDiffFile {
  id :: StorableWrap (CVector 20 CUChar),
  path :: CString,
  size :: CULong,
  flags :: CUInt,
  mode :: CUShort,
  id_abbrev :: CUShort
}
  deriving stock (Generic)
  deriving anyclass (CStorable)
  deriving (Storable) via (CStorableWrapper GitDiffFile)

data GitDiffDelta = GitDiffDelta {
  deltaDiffStatus :: !CUInt, -- git_delta_t
  deltaFlags :: !CUInt,
  deltaSimilarity :: !CUShort,
  deltaNfiles :: !CUShort,
  oldFile :: !GitDiffFile,
  newFile :: !GitDiffFile
}
  deriving stock (Generic)
  deriving anyclass (CStorable)
  deriving (Storable) via (CStorableWrapper GitDiffDelta)

data GitStatusEntry = GitStatusEntry {
  status :: !CUInt,
  headToIndex :: !(Ptr GitDiffDelta),
  indexToWorkDir :: !(Ptr GitDiffDelta)
}
  deriving stock (Generic, Show)
  deriving anyclass (CStorable)
  deriving (Storable) via (CStorableWrapper GitStatusEntry)


-- | Status functions:

-- int git_status_list_new_integr(git_status_list **out, git_repository *repo)
foreign import ccall "git_integr.h git_status_list_new_integr" c_git_status_list_new_integr :: Ptr (Ptr GitStatusList) -> Ptr GitRepo -> IO CInt

-- const git_status_entry * git_status_byindex(git_status_list *statuslist, size_t idx);
foreign import ccall "git2/status.h git_status_byindex" c_git_status_byindex :: Ptr GitStatusList -> CSize -> IO (Ptr GitStatusEntry)

-- size_t git_status_list_entrycount(git_status_list *statuslist);
foreign import ccall "git2/status.h git_status_list_entrycount" c_git_status_list_entrycount :: Ptr GitStatusList -> IO CSize

-- void git_status_list_free(git_status_list *statuslist);
foreign import ccall "git2/status.h git_status_list_free" c_git_status_list_free :: Ptr GitStatusList -> IO ()


-- | Wrappers

repoStatus :: GitRepoPtr -> IO StatusInfo
repoStatus ptr = do
  p <- malloc
  c_git_status_list_new_integr p ptr
  statusListPtr <- peek p
  count <- c_git_status_list_entrycount statusListPtr
  entries <- loop statusListPtr count 0
  deltaInfos <- mapM mapStatusEntry entries
  c_git_status_list_free statusListPtr
  free p
  pure (StatusInfo deltaInfos)
  where
    loop :: Ptr GitStatusList -> CSize -> CSize -> IO [GitStatusEntry]
    loop listPtr count i = if i == count then pure [] else do
      entryPtr <- c_git_status_byindex listPtr i
      entry <- peek entryPtr
      rest <- loop listPtr count (i + 1)
      pure (entry : rest)

mapStatusEntry :: GitStatusEntry -> IO StatusEntryDeltaInfo
mapStatusEntry (GitStatusEntry _ h2i i2w) = do
  h2iPaths <- mapDeltaPtr h2i
  i2wPaths <- mapDeltaPtr i2w
  pure $ StatusEntryDeltaInfo h2iPaths i2wPaths

mapDeltaPtr :: Ptr GitDiffDelta -> IO (Maybe DeltaInfo)
mapDeltaPtr ptr = if ptr /= nullPtr
  then do
    delta <- peek ptr
    paths <- mapDelta delta
    pure (Just paths)
  else pure Nothing

mapDelta :: GitDiffDelta -> IO DeltaInfo
mapDelta (GitDiffDelta s _ _ _ (GitDiffFile _ p1 _ _ _ _) (GitDiffFile _ p2 _ _ _ _)) = do
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
mapStatus _  = error "check LibGit integration"


sizeOfGitDiffFile :: Int
sizeOfGitDiffFile = sizeOf (undefined :: GitDiffFile)

sizeOfGitDiffDelta :: Int
sizeOfGitDiffDelta = sizeOf (undefined :: GitDiffDelta)