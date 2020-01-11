module Foreign.LibGit.AnnotatedCommit (
  GitAnnotatedCommit(..),
  c_git_annotated_commit_ref,
  c_git_annotated_commit_from_ref,
  c_git_annotated_commit_free,
  c_git_annotated_commit_id
) where

import System.IO (IO)
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Foreign.C.String (CString)
import Foreign.CStorable (CStorable)
import Foreign.C.Types
import Foreign.LibGit.Models


data GitAnnotatedCommit = GitAnnotatedCommit
  deriving stock (Generic)
  deriving anyclass (CStorable)

-- const char * git_annotated_commit_ref(const git_annotated_commit *commit);
foreign import ccall "git2/annotated_commit.h git_annotated_commit_ref" c_git_annotated_commit_ref :: Ptr GitAnnotatedCommit -> IO CString

-- int git_annotated_commit_from_ref(git_annotated_commit **out, git_repository *repo, const git_reference *ref);
foreign import ccall "git2/annotated_commit.h git_annotated_commit_from_ref" c_git_annotated_commit_from_ref :: Ptr (Ptr GitAnnotatedCommit) -> GitRepoPtr -> GitRefPtr -> IO CInt

-- void git_annotated_commit_free(git_annotated_commit *commit);
foreign import ccall "git2/annotated_commit.h git_annotated_commit_free" c_git_annotated_commit_free :: Ptr GitAnnotatedCommit -> IO ()

-- const git_oid * git_annotated_commit_id(const git_annotated_commit *commit);
foreign import ccall "git2/annotated_commit.h git_annotated_commit_id" c_git_annotated_commit_id :: Ptr GitAnnotatedCommit -> IO (Ptr GitOid)
