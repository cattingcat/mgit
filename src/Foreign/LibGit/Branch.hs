module Foreign.LibGit.Branch where

import System.IO (IO)

import Foreign
import Foreign.C.Types
import Foreign.C.String (CString)
import Foreign.CStorable
import Foreign.CStorableWrap
import Foreign.LibGit.Models
import Foreign.LibGit.AnnotatedCommit

import GHC.Generics (Generic)
import GHC.Show (Show(..))
import GHC.Base (Eq(..))


-- git_branch_iterator
data GitBranchIterator = GitBranchIterator
  deriving stock (Generic)
  deriving anyclass (CStorable)

-- git_branch_t
newtype GitBranchType = GitBranchType CUChar
  deriving stock (Eq, Show, Generic)
  deriving newtype CStorable
  deriving Storable via (CStorableWrapper GitBranchType)
  
  
-- int git_branch_iterator_new(git_branch_iterator **out, git_repository *repo, git_branch_t list_flags);
foreign import ccall "git2/branch.h git_branch_iterator_new" c_git_branch_iterator_new :: Ptr (Ptr GitBranchIterator) -> Ptr GitRepo -> GitBranchType -> IO CInt

-- void git_branch_iterator_free(git_branch_iterator *iter);
foreign import ccall "git2/branch.h git_branch_iterator_free" c_git_branch_iterator_free :: Ptr GitBranchIterator -> IO ()

-- int git_branch_next(git_reference **out, git_branch_t *out_type, git_branch_iterator *iter);
foreign import ccall "git2/branch.h git_branch_next" c_git_branch_next :: Ptr GitRefPtr -> Ptr GitBranchType -> Ptr GitBranchIterator -> IO CInt

-- int git_branch_name(const char **out, const git_reference *ref);
foreign import ccall "git2/branch.h git_branch_name" c_git_branch_name :: Ptr CString -> GitRefPtr -> IO CInt

-- int git_branch_is_head(const git_reference *branch);
foreign import ccall "git2/branch.h git_branch_is_head" c_git_branch_is_head :: GitRefPtr -> IO CInt

-- int git_branch_create_from_annotated(git_reference **ref_out, git_repository *repository, const char *branch_name, const git_annotated_commit *commit, int force);
foreign import ccall "git2/branch.h git_branch_create_from_annotated" c_git_branch_create_from_annotated:: Ptr GitRefPtr -> GitRepoPtr -> CString -> Ptr GitAnnotatedCommit -> CInt-> IO CInt
