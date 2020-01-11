module Foreign.LibGit.Repository (
  c_git_repository_commondir,
  c_git_repository_set_head 
) where

import System.IO (IO)
import Foreign.C.Types
import Foreign.C.String (CString)
import Foreign.LibGit.Models


-- const char * git_repository_commondir(const git_repository *repo);
foreign import ccall "git2/repository.h git_repository_commondir" c_git_repository_commondir :: GitRepoPtr -> IO CString

-- int git_repository_set_head(git_repository *repo, const char *refname);
foreign import ccall "git2/repository.h git_repository_set_head" c_git_repository_set_head :: GitRepoPtr -> CString-> IO CInt
