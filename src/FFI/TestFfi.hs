{-# language ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module FFI.TestFfi where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable
import GHC.Generics (Generic)
import System.Directory
import FFI.CArray
import qualified LibGit.Status as S
import qualified Data.Array.Base as A


data MyStruct = MyStruct {
  arr :: StorableWrap (CArray 5 CInt),
  str :: CString
} deriving (Generic, CStorable)

instance Storable MyStruct where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

foreign import ccall "print_kek.h foo" c_foo :: CInt -> IO (Ptr MyStruct)
foreign import ccall "print_kek.h bar" c_bar :: Ptr MyStruct -> IO ()
foreign import ccall "print_kek.h test_status_size" c_test_status_size :: IO ()


testCArrayFfi :: IO ()
testCArrayFfi = do
  msPtr <- c_foo 5
  (MyStruct (Storable (CArray ms)) cstr) <- peek msPtr
  r0 <- A.readArray ms 0
  r1 <- A.readArray ms 1
  r2 <- A.readArray ms 2
  r3 <- A.readArray ms 3
  r4 <- A.readArray ms 4
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
  arr <- A.newArray (0, 4) 5
  withCString "kek puk" $ \s -> do
    let ms = MyStruct (Storable (CArray arr)) s
    ptr <- malloc
    poke ptr ms 
    c_bar ptr

testStatusEnumSize :: IO ()
testStatusEnumSize = do 
  c_test_status_size
  print $ "GitDiffFile size : " ++ show (sizeOf (undefined :: S.GitDiffFile))
  print $ "GitDiffDelta size : " ++ show (sizeOf (undefined :: S.GitDiffDelta))