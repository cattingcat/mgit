module LibGit.Test where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable
import GHC.Generics (Generic)
import System.Directory
import LibGit.LibGit
import LibGit.Models
import LibGit.GitFileStatusShow

import qualified LibGit.GitFileStatus as GFS

tstRepoOpen = do
  print "Test repo open"
  pwd <- getCurrentDirectory
  inir <- c_git_libgit2_init
  print inir
  p <- mallocBytes (sizeOf (undefined :: Ptr (Ptr GitRepo)))
  withCString pwd $ \s -> do
    r <- c_git_repository_open p s
    print r
  repoPtr <- peek p
  repoPathPtr <- c_git_repository_commondir repoPtr
  repoPath <- peekCString repoPathPtr
  print repoPath
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

tstRepoStatus = do
  print "Test repo status"
  pwd <- getCurrentDirectory
  inir <- c_git_libgit2_init
  p <- mallocBytes (sizeOf (undefined :: Ptr (Ptr GitRepo)))
  withCString pwd $ \s -> do
    r <- c_git_repository_open p s
    print r
  repoPtr <- peek p
  cb <- wrapGitStatusCb statusFunc
  r <- c_git_status_foreach_integr repoPtr cb nullPtr
  free p
  
tstRepoOrigin = do
  print "Test repo status"
  pwd <- getCurrentDirectory
  inir <- c_git_libgit2_init
  ppRepo <- mallocBytes (sizeOf (undefined :: Ptr GitRepo))
  withCString pwd $ \s -> do
    r <- c_git_repository_open ppRepo s
    print r
  repoPtr <- peek ppRepo
  cb <- wrapGitStatusCb statusFunc
  ppRemote <- mallocBytes (sizeOf (undefined :: Ptr GitRemote))
  withCString "origin" $ \s -> do
    r <- c_git_remote_lookup ppRemote repoPtr s
    print r
  remotePtr <- peek ppRemote
  uriStr <- c_git_remote_url remotePtr
  s <- peekCString uriStr
  print s
  free ppRepo
  free ppRemote

tstFetch = do
  print "Test fetch"
  pwd <- getCurrentDirectory
  inir <- c_git_libgit2_init
  ppRepo <- mallocBytes (sizeOf (undefined :: Ptr GitRepo))
  withCString pwd $ \s -> do
    r <- c_git_repository_open ppRepo s
    print r
  repoPtr <- peek ppRepo
  cb <- wrapGitStatusCb statusFunc
  ppRemote <- mallocBytes (sizeOf (undefined :: Ptr GitRemote))
  withCString "origin" $ \s -> do
    r <- c_git_remote_lookup ppRemote repoPtr s
    print r
  remotePtr <- peek ppRemote
  
  ppFetchOpts <- mallocBytes (sizeOf (undefined :: Ptr GitFetchOptions))
  do 
    r <- c_git_fetch_init_options_integr ppFetchOpts
    print r
  fetchOptsPtr <- peek ppFetchOpts
  do 
    refs <- makeStrArr ["master"]
    r <- c_git_remote_download remotePtr refs fetchOptsPtr
    print r
  free ppFetchOpts
  free ppRepo
  free ppRemote

statusFunc :: GitStatusCb
statusFunc cstr fileStat _ =
  if fileStat == GFS.ignored
    then pure 0
    else do
      str <- peekCString cstr
      print (show fileStat ++ " :   " ++ str)
      pure 0

printVer :: CInt -> CInt -> CInt -> String
printVer mj mn pc = show mj ++ "." ++ show mn ++ "." ++ show pc