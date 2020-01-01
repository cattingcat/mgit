module LibGit.Refs where

import Foreign.Ptr
import Foreign.C.Types
import LibGit.Models

-- int git_reference_is_branch(const git_reference *ref);
foreign import ccall "git2/refs.h git_reference_is_branch" c_git_reference_is_branch :: Ptr GitReference -> IO CInt

isBranch :: Ptr GitReference -> IO Bool
isBranch ptr = do
  r <- c_git_reference_is_branch ptr
  pure (r == 1)