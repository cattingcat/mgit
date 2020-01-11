{-# LANGUAGE UndecidableInstances #-}

module LibGit.Remote (
  GitRemote(..),
  GitRemotePtr,
  lookupRemote,
  remoteUri,
  remoteFetch
) where

import System.IO (IO)

import Control.Category ((.))
import Control.Applicative (pure)
import Data.Text
import Data.Text.Foreign
import Data.Tuple (fst)
import Data.Functor ((<$>))

import Foreign
import Foreign.C.String (peekCString)
import Foreign.LibGit.Models
import Foreign.LibGit.Remote


lookupRemote :: GitRepoPtr -> Text -> (GitRemotePtr -> IO a) -> IO a
lookupRemote repo name f = do
  p <- malloc
  _ <- withCStringLen name (c_git_remote_lookup p repo . fst)
  -- todo: ^ check res
  remotePtr <- peek p
  res <- f remotePtr
  c_git_remote_free remotePtr
  free p
  pure res
  
remoteUri :: GitRemotePtr -> IO Text
remoteUri remote = do 
  uriStr <- c_git_remote_url remote
  pack <$> peekCString uriStr

remoteFetch :: GitRemotePtr -> IO ()
remoteFetch remote = do
  p <- malloc
  c_git_fetch_init_options_integr p
  optsPtr <- peek p
  _ <- c_git_remote_fetch remote nullPtr optsPtr nullPtr
  -- todo: ^ check res
  free optsPtr -- todo: check
  free p