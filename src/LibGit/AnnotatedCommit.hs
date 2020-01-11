module LibGit.AnnotatedCommit (
  GitAnnotatedCommit(..),
  annotatedCommitName,
  getAnnotatedCommit,
  freeAnnotatedCommit,
  commitId
) where

import System.IO (IO)
import Control.Applicative
import Data.Text

import Foreign
import Foreign.C.String (peekCString)
import Foreign.LibGit.Models
import Foreign.LibGit.AnnotatedCommit


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