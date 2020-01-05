{-# language ForeignFunctionInterface #-}

module LibGit.Checkout where

import Foreign
import Foreign.C.Types

import LibGit.Models

-- int git_checkout_head(git_repository *repo, const git_checkout_options *opts);
-- int git_checkout_head_integr(git_repository *repo);
foreign import ccall "git_integr.h git_checkout_head_integr" c_git_checkout_head_integr :: GitRepoPtr -> IO CInt