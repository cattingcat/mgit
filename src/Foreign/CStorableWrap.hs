module Foreign.CStorableWrap (
  CStorableWrapper(..)
) where

import Control.Category
import Data.Functor

import Foreign
import Foreign.CStorable


-- | Use deriving via with this wrapper to get Storable instance
newtype CStorableWrapper a = WrapCStorable { unCSWrap :: a }

instance CStorable a => Storable (CStorableWrapper a) where
  sizeOf = cSizeOf . unCSWrap
  alignment = cAlignment . unCSWrap
  poke ptr (WrapCStorable a) = cPoke (castPtr ptr) a
  peek ptr = fmap WrapCStorable (cPeek (castPtr ptr))
