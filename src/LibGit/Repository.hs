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

import Foreign.C.String (peekCString)
import Foreign.LibGit.Models
import Foreign.LibGit.Repository


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