{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module LibGit.Status where

import System.Directory
import LibGit.Models
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable
import Foreign.Storable
import LibGit.GitStatus
import GHC.Generics (Generic)
import FFI.CArray
import FFI.Storable


data GitStatusList = GitStatusList
  deriving (Generic, CStorable)

data GitDiffFile = GitDiffFile {
  id :: StorableWrap (CArray 20 CUChar),
  path :: CString,
  size :: CULong,
  flags :: CUInt,
  mode :: CUShort,
  id_abbrev :: CUShort
}
  deriving (Generic, CStorable)
  deriving (Storable) via (CStorableWrapper GitDiffFile)

data GitDiffDelta = GitDiffDelta {
  deltaDiffStatus :: !CUInt, -- git_delta_t
  deltaFlags :: !CUInt,
  deltaSimilarity :: !CUShort,
  deltaNfiles :: !CUShort,
  oldFile :: !GitDiffFile,
  newFile :: !GitDiffFile
}
  deriving (Generic, CStorable)
  deriving (Storable) via (CStorableWrapper GitDiffDelta)

data GitStatusEntry = GitStatusEntry {
  status :: !CUInt,
  headToIndex :: !(Ptr GitDiffDelta),
  indexToWorkDir :: !(Ptr GitDiffDelta)
}
  deriving (Generic, CStorable, Show)
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