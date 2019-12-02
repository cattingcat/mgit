
module NativeGit where
{-
--import qualified Bindings.Libgit2.Types as T
--import qualified Bindings.Libgit2.Status as S
import qualified Bindings.Libgit2 as LG2
import GHC.Ptr

import qualified Foreign.Concurrent as FC
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.ForeignPtr
import Control.Monad (when)
import System.Process(system)
import Data.Text (Text, pack, unpack)

--tst1 = T.C'git_repository
--
--tst2 = S.c'git_status_file

--tst3 = LG2.c'git_repository_open

gitStrArray2List :: Ptr LG2.C'git_strarray -> IO [Text]
gitStrArray2List gitStrs = do
  count <- fromIntegral <$> peek (LG2.p'git_strarray'count gitStrs)
  strings <- peek $ LG2.p'git_strarray'strings gitStrs

  r0 <- Foreign.Marshal.Array.peekArray count strings
  r1 <- sequence $ fmap peekCString r0
  return $ fmap pack r1


tst4 = LG2.withLibGitDo $ do
--    system "git init smoke.git"
    alloca $ \repoPtrPtr ->
      withCString "smoke.git/.git" $ \str -> do
        r <- LG2.c'git_repository_open repoPtrPtr str
        when (r < 0) $ error ("ErrNo: " ++ show r)

        repoPtr <- peek repoPtrPtr
        tagListPrt <- malloc
        r1 <- LG2.c'git_tag_list tagListPrt repoPtr
        when (r1 < 0) $ error ("git_tag_list ErrNo: " ++ show r1)

--        tagList <- peek tagListPrt
        txt <- gitStrArray2List tagListPrt
        print txt

        free tagListPrt
        peek repoPtrPtr >>= LG2.c'git_repository_free

getGitRepoPtr :: FilePath -> IO (ForeignPtr LG2.C'git_repository)
getGitRepoPtr p = LG2.withLibGitDo $ do
  ptr <- malloc
  res <- withCString p (LG2.c'git_repository_open ptr)
  when (res < 0) $ error "error while repo opening"
  fptr <- do
    ptr' <- peek ptr
    FC.newForeignPtr ptr' (LG2.c'git_repository_free ptr')
  pure fptr

tst5 = getGitRepoPtr "smoke.git/.git"


tst6 = LG2.withLibGitDo $
--    system "git init smoke.git"
    alloca $ \repoPtrPtr ->
      withCString "smoke.git/.git" $ \str -> do
        r <- LG2.c'git_repository_open repoPtrPtr str
        when (r < 0) $ error ("ErrNo: " ++ show r)

        repoPtr <- peek repoPtrPtr

        cb <- callback
        r1 <- LG2.c'git_status_foreach repoPtr cb nullPtr
        when (r1 < 0) $ error ("git_tag_list ErrNo: " ++ show r1)

        peek repoPtrPtr >>= LG2.c'git_repository_free


foreign import ccall "wrapper"
  wrapStatusCallback :: (CString -> CUInt -> Ptr () -> IO CInt) -> IO (FunPtr (CString -> CUInt -> Ptr () -> IO CInt))

callback :: IO LG2.C'git_status_cb
callback = wrapStatusCallback cb where
  cb cstr (CUInt cint) nullp = do
    str <- peekCString cstr
    print str
    print cint
    pure $ CInt 0


--tst6 :: FilePath -> IO ()
--tst6 p = do
--  repoPtr <- getGitRepoPtr p
--  repoStruct <-
--  ptr <- malloc
--  r <- LG2.c'git_tag_list ptr
--
--  pure ()


--processRepo :: Ptr LG2.C'git_repository -> IO ()
--processRepo repo = do
--  r <- peek repo
--
--  LG2.c'git_repository_free repo
-}
