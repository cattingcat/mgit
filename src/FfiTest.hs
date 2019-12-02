{-# language ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module FfiTest where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable
import GHC.Generics (Generic)

-- | more details here: https://wiki.haskell.org/Foreign_Function_Interface


-- | import C-functions
foreign import ccall "math.h sin" c_sin :: CDouble -> CDouble

-- | Take function pointer with &
foreign import ccall "math.h &exp" a_exp :: FunPtr (CDouble -> CDouble)

-- | Wrap function to pointer
foreign import ccall "wrapper" wrapDDFunc :: (CDouble -> CDouble) -> IO (FunPtr (CDouble -> CDouble))

-- | Unwrap function pointer
foreign import ccall "dynamic" unwrapDDFunc :: FunPtr (CDouble -> CDouble) -> (CDouble -> CDouble)

-- | Using C Strings
foreign import ccall "printf" c_echo :: CString -> IO ()



data Complex = MkComplex Double Double
  deriving (Show)

instance Storable Complex where
  sizeOf _ = 2 * sizeOf (0.0 :: Double)
  peek ptr = do
    re <- peekByteOff ptr 0
    im <- peekByteOff ptr (sizeOf re)
    pure (MkComplex re im)
  poke ptr (MkComplex re im) = do
    pokeByteOff ptr 0 re
    pokeByteOff ptr (sizeOf re) im
  alignment _ = sizeOf (0.0 :: Double)

newtype Pair = Pair Complex
  deriving (Storable, Show)


data MyStruct = MyStruct{ d :: Double, c :: Word8, i :: Int32 }
  deriving (Generic, Show)

instance CStorable MyStruct

instance Storable MyStruct where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek



five :: Double 
five = 5.0

tst1 = c_sin $ CDouble five


tst2 = unwrapDDFunc a_exp $ CDouble five

tst3 = do
  ptr <- mallocBytes 128
  poke ptr (MkComplex 5.0 3.0)
  complex <- peek ptr
  print complex
  free ptr

tst4 :: IO ()
tst4 = withCString "kekpuk" c_echo

tst5 :: IO ()
tst5 = do
  ptr <- mallocBytes 128
  poke ptr (MyStruct 5.0 3 55)
  ms <- peek ptr
  print ms
  free ptr
