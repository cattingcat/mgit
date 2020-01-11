module Foreign.LibGit.Common (
  c_git_libgit2_init,
  c_git_libgit2_shutdown,
  c_git_libgit2_version,
  c_git_repository_open,
  c_git_repository_free
) where

import System.IO (IO)
import Foreign
import Foreign.C.Types
import Foreign.C.String (CString)
import Foreign.LibGit.Models


-- int git_libgit2_init();
foreign import ccall "git2/global.h git_libgit2_init" c_git_libgit2_init :: IO CInt

-- int git_libgit2_shutdown();
foreign import ccall "git2/global.h git_libgit2_shutdown" c_git_libgit2_shutdown :: IO CInt


-- int git_libgit2_features();
--foreign import ccall "git2/common.h git_libgit2_features" c_git_libgit2_features :: IO CInt

-- void git_libgit2_version(int *major, int *minor, int *rev);
foreign import ccall "git2/common.h git_libgit2_version" c_git_libgit2_version :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()


-- int git_repository_open(git_repository **out, const char *path);
foreign import ccall "git2.h git_repository_open" c_git_repository_open :: Ptr (Ptr GitRepo) -> CString -> IO CInt

-- void git_repository_free(git_repository *repo);
foreign import ccall "git2.h git_repository_free" c_git_repository_free :: Ptr GitRepo -> IO ()

