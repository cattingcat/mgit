module FFI.Storable (
  CStorableWrapper(..)
) where

import Foreign
import Foreign.CStorable


-- | Use deriving via with this wrapper to get Storable instance
newtype CStorableWrapper a = CStorableWrapper { unCSWrap :: a }

instance CStorable a => Storable (CStorableWrapper a) where
  sizeOf = cSizeOf . unCSWrap
  alignment = cAlignment . unCSWrap
  poke ptr (CStorableWrapper a) = cPoke (castPtr ptr) a
  peek ptr = fmap CStorableWrapper (cPeek (castPtr ptr))
