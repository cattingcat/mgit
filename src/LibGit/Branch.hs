{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module LibGit.Branch where

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

-- git_branch_iterator
data GitBranchIterator = GitBranchIterator
  deriving (Generic, CStorable)

-- git_branch_t
newtype GitBranchType = GitBranchType CUChar
  deriving (Eq, Show, Generic, CStorable)
  deriving Storable via (StorableW GitBranchType)

-- git_reference
data GitReference = GitReference
  deriving (Generic, CStorable)

localBranch :: GitBranchType
localBranch = GitBranchType 1

remoteBranch :: GitBranchType
remoteBranch = GitBranchType 2

allBranches :: GitBranchType
allBranches = GitBranchType $ (1 :: CUChar) .|. (2 :: CUChar)

-- int git_branch_iterator_new(git_branch_iterator **out, git_repository *repo, git_branch_t list_flags);
foreign import ccall "git2/branch.h git_branch_iterator_new" c_git_branch_iterator_new :: Ptr (Ptr GitBranchIterator) -> Ptr GitRepo -> GitBranchType -> IO CInt

-- void git_branch_iterator_free(git_branch_iterator *iter);
foreign import ccall "git2/branch.h git_branch_iterator_free" c_git_branch_iterator_free :: Ptr GitBranchIterator -> IO ()

-- int git_branch_next(git_reference **out, git_branch_t *out_type, git_branch_iterator *iter);
foreign import ccall "git2/branch.h git_branch_next" c_git_branch_next :: Ptr (Ptr GitReference) -> Ptr GitBranchType -> Ptr GitBranchIterator -> IO CInt

-- int git_branch_name(const char **out, const git_reference *ref);
foreign import ccall "git2/branch.h git_branch_name" c_git_branch_name :: Ptr CString -> Ptr GitReference -> IO CInt

-- GIT_ITEROVER
iterOver :: CInt
iterOver = -31