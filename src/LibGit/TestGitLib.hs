module LibGit.TestGitLib where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable
import GHC.Generics (Generic)
import System.Directory
import LibGit.Common
import LibGit.Models
import qualified LibGit.Status as S
import LibGit.Wrappers
import LibGit.GitFileStatusShow

import qualified LibGit.GitFileStatus as GFS
import Control.Monad (when)


--tstFetch = do
--  print "Test fetch"
--  pwd <- getCurrentDirectory
--  inir <- c_git_libgit2_init
--  ppRepo <- mallocBytes (sizeOf (undefined :: Ptr GitRepo))
--  withCString pwd $ \s -> do
--    r <- c_git_repository_open ppRepo s
--    print r
--  repoPtr <- peek ppRepo
--  cb <- wrapGitStatusCb statusFunc
--  ppRemote <- mallocBytes (sizeOf (undefined :: Ptr GitRemote))
--  withCString "origin" $ \s -> do
--    r <- c_git_remote_lookup ppRemote repoPtr s
--    print r
--  remotePtr <- peek ppRemote
--  
--  ppFetchOpts <- mallocBytes (sizeOf (undefined :: Ptr GitFetchOptions))
--  do 
--    r <- c_git_fetch_init_options_integr ppFetchOpts
--    print r
--  fetchOptsPtr <- peek ppFetchOpts
--  do 
--    refs <- makeStrArr ["master"]
--    r <- c_git_remote_download remotePtr refs fetchOptsPtr
--    print r
--  free ppFetchOpts
--  free ppRepo
--  free ppRemote


tstStatus = withLibGit $ do
  pwd <- getCurrentDirectory
  withRepo pwd $ \repo -> do
    statusInfo <- repoStatus repo
    print statusInfo

tstRemote = withLibGit $ do
  pwd <- getCurrentDirectory
  withRepo pwd $ \repo ->
    lookupRemote repo "origin" $ \r -> do
      uri <- remoteUri r
      print uri

tstRemoteFetch = withLibGit $ do
  pwd <- getCurrentDirectory
  withRepo pwd $ \repo ->
    lookupRemote repo "origin" remoteFetch
    
tstBranches = withLibGit $ do
  pwd <- getCurrentDirectory
  withRepo pwd $ \repo ->
    branches repo