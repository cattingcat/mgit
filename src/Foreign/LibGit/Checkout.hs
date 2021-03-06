module Foreign.LibGit.Checkout (
  c_git_checkout_head_integr,
  c_git_checkout_tree_integr
) where

import System.IO (IO)

import Foreign
import Foreign.C.Types
import Foreign.LibGit.Models


-- int git_checkout_head(git_repository *repo, const git_checkout_options *opts);
-- int git_checkout_head_integr(git_repository *repo);
foreign import ccall "git_integr.h git_checkout_head_integr" c_git_checkout_head_integr :: GitRepoPtr -> IO CInt

-- int git_checkout_tree(git_repository *repo, const git_object *treeish, const git_checkout_options *opts);
-- int git_checkout_tree_integr(git_repository *repo, const git_object *treeish);
foreign import ccall "git_integr.h git_checkout_tree_integr" c_git_checkout_tree_integr :: GitRepoPtr -> Ptr GitCommit -> IO CInt