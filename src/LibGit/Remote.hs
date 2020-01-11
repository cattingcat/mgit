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

import GHC.Generics (Generic)

import Foreign
import Foreign.C.Types
import Foreign.C.String hiding (withCStringLen)
import Foreign.CStorable
import LibGit.Models


data GitRemote = GitRemote
  deriving stock (Generic)
  deriving anyclass (CStorable)

type GitRemotePtr = Ptr GitRemote

data GitFetchOptions = GitFetchOptions
  deriving stock (Generic)
  deriving anyclass (CStorable)
  
  
-- int git_remote_list(git_strarray *out, git_repository *repo);

-- int git_remote_lookup(git_remote **out, git_repository *repo, const char *name);
foreign import ccall "git2/remote.h git_remote_lookup" c_git_remote_lookup :: Ptr (Ptr GitRemote) -> Ptr GitRepo -> CString -> IO CInt

-- void git_remote_free(git_remote *remote);
foreign import ccall "git2/remote.h git_remote_free" c_git_remote_free :: Ptr GitRemote -> IO ()

-- const char * git_remote_url(const git_remote *remote);
foreign import ccall "git2/remote.h git_remote_url" c_git_remote_url :: Ptr GitRemote -> IO CString

-- int git_remote_download(git_remote *remote, const git_strarray *refspecs, const git_fetch_options *opts);
--foreign import ccall "git2/remote.h git_remote_download" c_git_remote_download :: Ptr GitRemote -> Ptr GitStrArr -> Ptr GitFetchOptions -> IO CInt

-- int git_remote_fetch(git_remote *remote, const git_strarray *refspecs, const git_fetch_options *opts, const char *reflog_message);
foreign import ccall "git2/remote.h git_remote_fetch" c_git_remote_fetch :: Ptr GitRemote -> Ptr GitStrArr -> Ptr GitFetchOptions -> CString -> IO CInt

-- int git_fetch_init_options_integr(git_fetch_options *opts);
foreign import ccall "git_integr.h git_fetch_init_options_integr" c_git_fetch_init_options_integr :: Ptr (Ptr GitFetchOptions) -> IO CInt



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