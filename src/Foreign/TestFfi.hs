{-# language ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Foreign.TestFfi where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable
import GHC.Generics (Generic)
import System.Directory
import Foreign.CVector
import Foreign.CStorableWrap
import qualified LibGit.Status as S
import Data.Vector as V hiding ((++))


data MyStruct = MyStruct {
  arr :: StorableWrap (CVector 5 CInt),
  str :: CString
}
  deriving (Generic, CStorable)
  deriving Storable via (CStorableWrapper MyStruct)

foreign import ccall "print_kek.h foo" c_foo :: CInt -> IO (Ptr MyStruct)
foreign import ccall "print_kek.h bar" c_bar :: Ptr MyStruct -> IO ()
foreign import ccall "print_kek.h test_status_size" c_test_status_size :: IO ()


testCArrayFfi :: IO ()
testCArrayFfi = do
  msPtr <- c_foo 5
  (MyStruct (Storable (CVector ms)) cstr) <- peek msPtr
  let 
    r0 = ms ! 0
    r1 = ms ! 1
    r2 = ms ! 2
    r3 = ms ! 3
    r4 = ms ! 4
  str <- peekCString cstr
--  r5 <- readArray ms 5
--  r6 <- readArray ms 6
  print r0
  print r1
  print r2
  print r3
  print r4
  print str
--  print r5
  pure ()
  
testCArrayFfi2 :: IO ()
testCArrayFfi2 = do
  let arr = V.replicate 5 5
  withCString "kek puk" $ \s -> do
    let ms = MyStruct (Storable (CVector arr)) s
    ptr <- malloc
    poke ptr ms 
    c_bar ptr

testStatusEnumSize :: IO ()
testStatusEnumSize = do 
  c_test_status_size
  putStrLn $ "GitDiffFile size : " ++ show S.sizeOfGitDiffFile
  putStrLn $ "GitDiffDelta size : " ++ show S.sizeOfGitDiffDelta