{-# LANGUAGE BangPatterns #-}

module FFI.MyStruct where

#include "my_struct.h"

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable

data MyStruct = MyStruct { d :: !Double, c :: !Word8, i :: !Int32 }
  deriving (Show)

instance Storable MyStruct where
  sizeOf    _ = #{size MyStruct}
  alignment _ = #{alignment MyStruct}
  peek ptr = MyStruct
    <$> (#peek MyStruct, d) ptr
    <*> (#peek MyStruct, c) ptr
    <*> (#peek MyStruct, i) ptr
  poke ptr (MyStruct df cf ifl) = do
    poke (#{ptr MyStruct, d} ptr) df
    poke (#{ptr MyStruct, c} ptr) cf
    poke (#{ptr MyStruct, i} ptr) ifl