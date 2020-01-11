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

import Data.Maybe
import Data.Eq
import Data.Function (($))

import GHC.Err (error)
import GHC.Base (undefined)
import GHC.Num

import Foreign
import Foreign.C.Types
import Foreign.C.String (peekCString)
import Foreign.LibGit.Models
import Foreign.LibGit.Status

import MGit.StatusModels


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