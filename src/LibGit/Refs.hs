module LibGit.Refs (
  isBranch,
  isRemote,
  isTag,

  refName,

  formatRemoteRef,
  formatLocalRef,

  lookupRef,
  freeRef
) where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import LibGit.Models

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


isBranch :: GitRefPtr -> IO Bool
isBranch ptr = do
  r <- c_git_reference_is_branch ptr
  pure (r == 1)

isRemote :: GitRefPtr -> IO Bool
isRemote ptr = do
  r <- c_git_reference_is_remote ptr
  pure (r == 1)

isTag :: GitRefPtr -> IO Bool
isTag ptr = do
  r <- c_git_reference_is_tag ptr
  pure (r == 1)

refName :: GitRefPtr -> IO String
refName ptr = do
  cStr <- c_git_reference_name ptr
  peekCString cStr

formatRemoteRef :: String -> String -> String
formatRemoteRef origin branch = "refs/remotes/" <> origin <> "/" <> branch

formatLocalRef :: String -> String
formatLocalRef branch = "refs/heads/" <> branch

formatTagRef ::  String -> String
formatTagRef tag = "refs/tags/" <> tag

lookupRef :: GitRepoPtr -> String -> IO (Maybe GitRefPtr)
lookupRef ptr name = withCString name $ \str -> do
  p <- malloc
  r <- c_git_reference_lookup p ptr str
  if r == 0
  then do
    refPtr <- peek p
    free p
    pure $ Just refPtr
  else do
    free p
    pure Nothing

freeRef :: GitRefPtr -> IO ()
freeRef = c_git_reference_free