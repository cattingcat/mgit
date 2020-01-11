module LibGit.Refs (
  isBranch,
  isRemote,
  isTag,

  refName,

  formatRemoteRef,
  formatLocalRef,
  formatTagRef,

  lookupRef,
  freeRef
) where

import System.IO (IO)
import Control.Applicative
import Data.Eq
import Data.Bool
import Data.Maybe
import Data.Text
import Data.Text.Foreign
import Data.Monoid
import Data.Function

import Foreign
import Foreign.C.String (peekCString)
import Foreign.LibGit.Models
import Foreign.LibGit.Refs


isBranch :: GitRefPtr -> IO Bool
isBranch ptr = do
  r <- c_git_reference_is_branch ptr
  pure (r == 1)

isRemote :: GitRefPtr -> IO Bool
isRemote ptr = do
  r <- c_git_reference_is_remote ptr
  pure (r == 1)

isTag :: GitRefPtr -> IO Bool
isTag ptr = do
  r <- c_git_reference_is_tag ptr
  pure (r == 1)

refName :: GitRefPtr -> IO Text
refName ptr = do
  cStr <- c_git_reference_name ptr
  pack <$> peekCString cStr

formatRemoteRef :: Text -> Text -> Text
formatRemoteRef origin branch = "refs/remotes/" <> origin <> "/" <> branch

formatLocalRef :: Text -> Text
formatLocalRef branch = "refs/heads/" <> branch

formatTagRef ::  Text -> Text
formatTagRef tag = "refs/tags/" <> tag

lookupRef :: GitRepoPtr -> Text -> IO (Maybe GitRefPtr)
lookupRef ptr name = withCStringLen name $ \(str, _) -> do
  p <- malloc
  r <- c_git_reference_lookup p ptr str
  if r == 0
  then do
    refPtr <- peek p
    free p
    pure $ Just refPtr
  else do
    free p
    pure Nothing

freeRef :: GitRefPtr -> IO ()
freeRef = c_git_reference_free