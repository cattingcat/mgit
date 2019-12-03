{-# language ForeignFunctionInterface, DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module LibGit.LibGit where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable
import GHC.Generics (Generic)
import System.Directory

import qualified LibGit.GitFileStatus as GFS


data GitRepo = GitRepo
  deriving (Generic, Show, CStorable)

instance Storable GitRepo where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

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


-- int git_status_foreach(git_repository *repo, git_status_cb callback, void *payload);
foreign import ccall "git_integr.h git_status_foreach_integr" c_git_status_foreach_integr :: Ptr GitRepo -> FunPtr GitStatusCb -> Ptr () -> IO CInt