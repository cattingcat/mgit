{-# language ForeignFunctionInterface #-}

module LibGit.Common (
  withLibGit,
  withRepo,
  withRepoSafe,
  openRepo,
  libGitVersion,
  repoDir
) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import LibGit.Models
import Control.Exception (Exception, throw)


-- int git_libgit2_init();
foreign import ccall "git2/global.h git_libgit2_init" c_git_libgit2_init :: IO CInt

-- int git_libgit2_shutdown();
foreign import ccall "git2/global.h git_libgit2_shutdown" c_git_libgit2_shutdown :: IO CInt


-- int git_libgit2_features();
foreign import ccall "git2/common.h git_libgit2_features" c_git_libgit2_features :: IO CInt

-- void git_libgit2_version(int *major, int *minor, int *rev);
foreign import ccall "git2/common.h git_libgit2_version" c_git_libgit2_version :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()


-- int git_repository_open(git_repository **out, const char *path);
foreign import ccall "git2.h git_repository_open" c_git_repository_open :: Ptr (Ptr GitRepo) -> CString -> IO CInt


-- const char * git_repository_commondir(const git_repository *repo);
foreign import ccall "git2/repository.h git_repository_commondir" c_git_repository_commondir :: Ptr GitRepo -> IO CString




withLibGit :: IO a -> IO a
withLibGit io = do
  c_git_libgit2_init
  a <- io
  c_git_libgit2_shutdown
  pure a
  
openRepo :: FilePath -> IO GitRepoPtr
openRepo path = do
  p <- mallocBytes (sizeOf (undefined :: Ptr (Ptr GitRepo)))
  r <- withCString path (c_git_repository_open p) 
  repoPtr <- peek p
  free p
  pure repoPtr

withRepoSafe :: FilePath -> (Maybe GitRepoPtr -> IO a) -> IO a
withRepoSafe path f = do
  p <- mallocBytes (sizeOf (undefined :: Ptr (Ptr GitRepo)))
  r <- withCString path (c_git_repository_open p)
  res <- if r /= 0
    then 
      f Nothing
    else do
      repoPtr <- peek p
      r <- f (Just repoPtr)
      free repoPtr
      pure r    
  free p
  pure res

withRepo :: FilePath -> (GitRepoPtr -> IO a) -> IO a
withRepo path f = do
  p <- mallocBytes (sizeOf (undefined :: Ptr (Ptr GitRepo)))
  r <- withCString path (c_git_repository_open p)
  if r /= 0
    then throw (OpenRepoError r)
    else pure ()
  repoPtr <- peek p
  res <- f repoPtr
  free p
  free repoPtr
  pure res

showVer :: CInt -> CInt -> CInt -> String
showVer mj mn pc = show mj ++ "." ++ show mn ++ "." ++ show pc

libGitVersion :: IO String
libGitVersion = do
  majorPtr <- mallocBytes (sizeOf (undefined :: Ptr CInt))
  minorPtr <- mallocBytes (sizeOf (undefined :: Ptr CInt))
  patchPtr <- mallocBytes (sizeOf (undefined :: Ptr CInt))
  c_git_libgit2_version majorPtr minorPtr patchPtr
  verStr <- showVer
    <$> peek majorPtr
    <*> peek minorPtr
    <*> peek patchPtr
  free majorPtr
  free minorPtr
  free patchPtr
  pure verStr

repoDir :: GitRepoPtr -> IO FilePath
repoDir ptr = do
  pathPtr <- c_git_repository_commondir ptr
  path <- peekCString pathPtr
  free pathPtr
  pure path