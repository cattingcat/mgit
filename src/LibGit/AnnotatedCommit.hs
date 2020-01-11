module LibGit.AnnotatedCommit (
  GitAnnotatedCommit(..),
  annotatedCommitName,
  getAnnotatedCommit,
  freeAnnotatedCommit,
  commitId
) where

import Prelude ()

import System.IO (IO)
import Control.Applicative
import Data.Text

import GHC.Generics (Generic)

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable (CStorable)

import LibGit.Models


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


annotatedCommitName :: Ptr GitAnnotatedCommit -> IO Text
annotatedCommitName commitPtr = do
  cstr <- c_git_annotated_commit_ref commitPtr
  pack <$> peekCString cstr

getAnnotatedCommit :: GitRepoPtr -> GitRefPtr -> IO (Ptr GitAnnotatedCommit)
getAnnotatedCommit repo ref = do
  p <- malloc
  _ <- c_git_annotated_commit_from_ref p repo ref
  resPtr <- peek p
  free p
  pure resPtr

freeAnnotatedCommit :: Ptr GitAnnotatedCommit -> IO ()
freeAnnotatedCommit = c_git_annotated_commit_free

commitId :: Ptr GitAnnotatedCommit -> IO (Ptr GitOid)
commitId = c_git_annotated_commit_id