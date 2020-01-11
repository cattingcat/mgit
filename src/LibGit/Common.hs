module LibGit.Common (
  withLibGit,
  withRepo,
  withRepoSafe,
  openRepo,
  libGitVersion
) where

import System.IO (IO)
import System.FilePath (FilePath)

import Data.Eq
import Data.Text
import Data.Maybe
import Data.Monoid
import Data.Function (($))
import Data.Typeable (Typeable)
import Text.Show (show)

import Control.Applicative
import Control.Exception (throw, Exception)
import GHC.Show (Show)

import Foreign
import Foreign.C.Types
import Foreign.C.String (withCString)
import Foreign.LibGit.Models
import Foreign.LibGit.Common


newtype OpenRepoError = OpenRepoError CInt 
  deriving stock (Show, Typeable)
instance Exception OpenRepoError

withLibGit :: IO a -> IO a
withLibGit io = do
  c_git_libgit2_init
  a <- io
  c_git_libgit2_shutdown
  pure a
  
openRepo :: FilePath -> IO GitRepoPtr
openRepo path = do
  p <- malloc
  _ <- withCString path (c_git_repository_open p)
  repoPtr <- peek p
  free p
  pure repoPtr

withRepoSafe :: FilePath -> (Maybe GitRepoPtr -> IO a) -> IO a
withRepoSafe path f = do
  p <- malloc
  openRes <- withCString path (c_git_repository_open p)
  res <- if openRes /= 0
    then 
      f Nothing
    else do
      repoPtr <- peek p
      res <- f (Just repoPtr)
      c_git_repository_free repoPtr
      pure res
  free p
  pure res

withRepo :: FilePath -> (GitRepoPtr -> IO a) -> IO a
withRepo path f = do
  p <- malloc
  r <- withCString path (c_git_repository_open p)
  if r /= 0
    then throw (OpenRepoError r)
    else pure ()
  repoPtr <- peek p
  res <- f repoPtr
  free p
  c_git_repository_free repoPtr
  pure res

showVer :: CInt -> CInt -> CInt -> Text
showVer mj mn pc = pack $ show mj <> "." <> show mn <> "." <> show pc

libGitVersion :: IO Text
libGitVersion = do
  majorPtr <- malloc
  minorPtr <- malloc
  patchPtr <- malloc
  c_git_libgit2_version majorPtr minorPtr patchPtr
  verStr <- showVer
    <$> peek majorPtr
    <*> peek minorPtr
    <*> peek patchPtr
  free majorPtr
  free minorPtr
  free patchPtr
  pure verStr
