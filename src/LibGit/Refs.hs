module LibGit.Refs (
  isBranch,
  refName
) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import LibGit.Models

-- int git_reference_is_branch(const git_reference *ref);
foreign import ccall "git2/refs.h git_reference_is_branch" c_git_reference_is_branch :: Ptr GitReference -> IO CInt

-- const char * git_reference_name(const git_reference *ref);
foreign import ccall "git2/refs.h git_reference_name" c_git_reference_name :: Ptr GitReference -> IO CString


isBranch :: Ptr GitReference -> IO Bool
isBranch ptr = do
  r <- c_git_reference_is_branch ptr
  pure (r == 1)

refName :: Ptr GitReference -> IO String
refName ptr = do
  cStr <- c_git_reference_name ptr
  peekCString cStr