{-# language ForeignFunctionInterface #-}

module LibGit.LibGit where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable
import GHC.Generics (Generic)
import System.Directory
import LibGit.Models

import qualified LibGit.GitFileStatus as GFS

-- int git_libgit2_init();
foreign import ccall "git2/global.h git_libgit2_init" c_git_libgit2_init :: IO CInt

-- int git_libgit2_features();
foreign import ccall "git2/common.h git_libgit2_features" c_git_libgit2_features :: IO CInt

-- void git_libgit2_version(int *major, int *minor, int *rev);
foreign import ccall "git2/common.h git_libgit2_version" c_git_libgit2_version :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

-- int git_repository_open(git_repository **out, const char *path);
foreign import ccall "git2.h git_repository_open" c_git_repository_open :: Ptr (Ptr GitRepo) -> CString -> IO CInt

-- const char * git_repository_commondir(const git_repository *repo);
foreign import ccall "git2/repository.h git_repository_commondir" c_git_repository_commondir :: Ptr GitRepo -> IO CString

-- int git_status_foreach(git_repository *repo, git_status_cb callback, void *payload);
foreign import ccall "git2/status.h git_status_foreach" c_git_status_foreach :: Ptr GitRepo -> FunPtr GitStatusCb -> Ptr () -> IO CInt

-- int git_status_cb(const char *path, unsigned int status_flags, void *payload);
type GitStatusCb = CString -> GFS.GitFileStatus -> Ptr () -> IO CInt

foreign import ccall "wrapper" wrapGitStatusCb :: GitStatusCb -> IO (FunPtr GitStatusCb)

-- int git_remote_lookup(git_remote **out, git_repository *repo, const char *name);
foreign import ccall "git2/remote.h git_remote_lookup" c_git_remote_lookup :: Ptr (Ptr GitRemote) -> Ptr GitRepo -> CString -> IO CInt

-- const char * git_remote_url(const git_remote *remote);
foreign import ccall "git2/remote.h git_remote_url" c_git_remote_url :: Ptr GitRemote -> IO CString

-- int git_remote_download(git_remote *remote, const git_strarray *refspecs, const git_fetch_options *opts);
foreign import ccall "git2/remote.h git_remote_download" c_git_remote_download :: Ptr GitRemote -> Ptr GitStrArr -> Ptr GitFetchOptions -> IO CInt


-- int git_status_foreach(git_repository *repo, git_status_cb callback, void *payload);
foreign import ccall "git_integr.h git_status_foreach_integr" c_git_status_foreach_integr :: Ptr GitRepo -> FunPtr GitStatusCb -> Ptr () -> IO CInt

-- int git_fetch_init_options_integr(git_fetch_options *opts);
foreign import ccall "git_integr.h git_fetch_init_options_integr" c_git_fetch_init_options_integr :: Ptr (Ptr GitFetchOptions) -> IO CInt
