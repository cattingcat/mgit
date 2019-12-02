{-# language ForeignFunctionInterface, DeriveGeneric #-}

module FFI.LibGit where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable
import GHC.Generics (Generic)
import System.Directory


data GitRepo = GitRepo
  deriving (Generic, Show)

instance CStorable GitRepo

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


tstRepoOpen = do
  print "Test repo open"
  pwd <- getCurrentDirectory
  inir <- c_git_libgit2_init
  print inir
  p <- mallocBytes (sizeOf (undefined :: Ptr (Ptr GitRepo)))
  withCString pwd $ \s -> do
    r <- c_git_repository_open p s
    print r
  free p

tstLibGitVersion = do
  print "Test LibGit version"
  majorPtr <- mallocBytes (sizeOf (undefined :: Ptr CInt))
  minorPtr <- mallocBytes (sizeOf (undefined :: Ptr CInt))
  patchPtr <- mallocBytes (sizeOf (undefined :: Ptr CInt))
  c_git_libgit2_version majorPtr minorPtr patchPtr
  verStr <- printVer
    <$> peek majorPtr
    <*> peek minorPtr
    <*> peek patchPtr
  print verStr
  free majorPtr
  free minorPtr
  free patchPtr


printVer :: CInt -> CInt -> CInt -> String
printVer mj mn pc = show mj ++ "." ++ show mn ++ "." ++ show pc