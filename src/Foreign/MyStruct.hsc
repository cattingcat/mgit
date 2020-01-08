module Foreign.MyStruct where

#include "test_ffi.h"

import Foreign
import Foreign.C.Types

-- https://downloads.haskell.org/~ghc/7.6.3/docs/html/users_guide/hsc2hs.html

data MyStruct = MyStruct { d :: !Double, c :: !Word8, i :: !Int32 }
  deriving stock (Show)

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

-- Binding to kek_t enum
newtype KekStatusOpt = MkKekStatusOpt CUInt
#{enum KekStatusOpt, MkKekStatusOpt
    , kekFst = KEK_FST
    , kekSnd = KEK_SND
    , kekThrd = KEK_THRD
    , kekFrth = KEK_FRTH}