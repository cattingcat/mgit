{-# language ForeignFunctionInterface #-}

module LibGit.Repository (
  repoDir,
  setHead
) where

import System.IO (IO)
import System.FilePath (FilePath)
import System.FilePath.Posix (splitDirectories, joinPath)

import Control.Applicative (pure)
import Data.Text (Text)
import Data.Text.Foreign (withCStringLen)
import Data.List (init)
import Data.Function (($))

import Foreign.C.Types
import Foreign.C.String (CString, peekCString)

import LibGit.Models


-- const char * git_repository_commondir(const git_repository *repo);
foreign import ccall "git2/repository.h git_repository_commondir" c_git_repository_commondir :: GitRepoPtr -> IO CString

-- int git_repository_set_head(git_repository *repo, const char *refname);
foreign import ccall "git2/repository.h git_repository_set_head" c_git_repository_set_head :: GitRepoPtr -> CString-> IO CInt


repoDir :: GitRepoPtr -> IO FilePath
repoDir ptr = do
  pathPtr <- c_git_repository_commondir ptr
  gitDirPath <- peekCString pathPtr
  let dirs = splitDirectories gitDirPath
  pure $ joinPath $ init dirs -- Remove /.git/ segment

setHead :: GitRepoPtr -> Text -> IO ()
setHead ptr refName =
  withCStringLen refName $ \(s, _) -> do
    _ <- c_git_repository_set_head ptr s
    pure ()