{-# language ForeignFunctionInterface #-}

module LibGit.Repository (
  repoDir,
  setHead
) where

import Control.Exception (Exception, throw)
import System.FilePath.Posix (splitDirectories, joinPath)

import Foreign
import Foreign.C.Types
import Foreign.C.String

import LibGit.Models
import qualified LibGit.Checkout as C


-- const char * git_repository_commondir(const git_repository *repo);
foreign import ccall "git2/repository.h git_repository_commondir" c_git_repository_commondir :: GitRepoPtr -> IO CString

-- int git_repository_set_head(git_repository *repo, const char *refname);
foreign import ccall "git2/repository.h git_repository_set_head" c_git_repository_set_head :: GitRepoPtr -> CString-> IO CInt


repoDir :: GitRepoPtr -> IO FilePath
repoDir ptr = do
  pathPtr <- c_git_repository_commondir ptr
  gitDirPath <- peekCString pathPtr
  let dirs = splitDirectories gitDirPath
  pure $ joinPath $ init dirs

setHead :: GitRepoPtr -> String -> IO ()
setHead ptr refName =
  withCString refName $ \s -> do
    r <- c_git_repository_set_head ptr s
    pure()