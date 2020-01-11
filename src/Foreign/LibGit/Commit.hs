module Foreign.LibGit.Commit (
  c_git_commit_lookup,
  c_git_commit_free,
  c_git_commit_message
) where

import System.IO (IO)
import Foreign.Ptr (Ptr)
import Foreign.LibGit.Models
import Foreign.C.Types
import Foreign.C.String (CString)


-- int git_commit_lookup(git_commit **commit, git_repository *repo, const git_oid *id);
foreign import ccall "git2/commit.h git_commit_lookup" c_git_commit_lookup :: Ptr (Ptr GitCommit) -> GitRepoPtr -> Ptr GitOid -> IO CInt

-- void git_commit_free(git_commit *commit);
foreign import ccall "git2/commit.h git_commit_free" c_git_commit_free :: Ptr GitCommit -> IO ()

-- const char * git_commit_message(const git_commit *commit);
foreign import ccall "git2/commit.h git_commit_message" c_git_commit_message :: Ptr GitCommit -> IO CString
