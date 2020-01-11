module LibGit.Commit (
  GitCommit,

  getCommit,
  freeCommit,
  commitMessage
) where

import System.IO (IO)
import Control.Applicative
import Data.Text

import Foreign
import Foreign.C.String (peekCString)
import Foreign.LibGit.Commit
import Foreign.LibGit.Models


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