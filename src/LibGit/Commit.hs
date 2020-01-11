module LibGit.Commit (
  GitCommit,

  getCommit,
  freeCommit,
  commitMessage
) where

import System.IO (IO)
import Control.Applicative
import Data.Text

import GHC.Generics (Generic)

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable (CStorable)

import LibGit.Models


data GitCommit = GitCommit
  deriving stock (Generic)
  deriving anyclass (CStorable)

-- int git_commit_lookup(git_commit **commit, git_repository *repo, const git_oid *id);
foreign import ccall "git2/commit.h git_commit_lookup" c_git_commit_lookup :: Ptr (Ptr GitCommit) -> GitRepoPtr -> Ptr GitOid -> IO CInt

-- void git_commit_free(git_commit *commit);
foreign import ccall "git2/commit.h git_commit_free" c_git_commit_free :: Ptr GitCommit -> IO ()

-- const char * git_commit_message(const git_commit *commit);
foreign import ccall "git2/commit.h git_commit_message" c_git_commit_message :: Ptr GitCommit -> IO CString


getCommit :: GitRepoPtr -> Ptr GitOid -> IO (Ptr GitCommit)
getCommit repo oid = do
  p <- malloc
  _ <- c_git_commit_lookup p repo oid
  commitPtr <- peek p
  free p
  pure commitPtr

freeCommit :: Ptr GitCommit -> IO ()
freeCommit = c_git_commit_free

commitMessage :: Ptr GitCommit -> IO Text
commitMessage ptr = do
  cstr <- c_git_commit_message ptr
  pack <$> peekCString cstr