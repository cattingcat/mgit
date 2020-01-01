module FFI.Storable where

import Foreign
import Foreign.CStorable
import Unsafe.Coerce (unsafeCoerce)

newtype StorableW a = StorableW { unStorableW :: a }

instance CStorable a => Storable (StorableW a) where
  sizeOf = cSizeOf . unStorableW
  alignment = cAlignment . unStorableW
  poke ptr (StorableW a) = cPoke (castPtr ptr) a 
  peek ptr = fmap StorableW (cPeek (castPtr ptr)) 
