module Foreign.LibGit.Refs (
  c_git_reference_is_branch,
  c_git_reference_is_remote,
  c_git_reference_is_tag,
  c_git_reference_name,
  c_git_reference_lookup,
  c_git_reference_free
) where

import System.IO (IO)
import Foreign
import Foreign.C.Types
import Foreign.C.String (CString)
import Foreign.LibGit.Models

-- | git show-ref

-- int git_reference_is_branch(const git_reference *ref);
foreign import ccall "git2/refs.h git_reference_is_branch" c_git_reference_is_branch :: GitRefPtr -> IO CInt

-- int git_reference_is_remote(const git_reference *ref);
foreign import ccall "git2/refs.h git_reference_is_remote" c_git_reference_is_remote :: GitRefPtr -> IO CInt

-- int git_reference_is_tag(const git_reference *ref);
foreign import ccall "git2/refs.h git_reference_is_tag" c_git_reference_is_tag :: GitRefPtr -> IO CInt

-- const char * git_reference_name(const git_reference *ref);
foreign import ccall "git2/refs.h git_reference_name" c_git_reference_name :: GitRefPtr -> IO CString

-- int git_reference_lookup(git_reference **out, git_repository *repo, const char *name);
foreign import ccall "git2/refs.h git_reference_lookup" c_git_reference_lookup :: Ptr GitRefPtr -> GitRepoPtr -> CString -> IO CInt

-- void git_reference_free(git_reference *ref);
foreign import ccall "git2/refs.h git_reference_free" c_git_reference_free :: GitRefPtr -> IO ()

