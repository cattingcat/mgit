module Foreign.CVector (
  CVector(..)
) where

import GHC.Base
import GHC.TypeNats
import GHC.Natural
import GHC.Num

import Data.Data
import Data.Vector
import Data.Vector.Mutable

import Foreign.Ptr
import Foreign.Storable


newtype CVector (n :: Nat) (a :: Type) = CVector (Vector a)

instance (Storable a, KnownNat n) => Storable (CVector n a) where
  sizeOf :: CVector n a -> Int
  sizeOf _ = intVal @n * sizeOf (undefined :: a)

  peek :: Ptr (CVector n a) -> IO (CVector n a)
  peek = peekCVector

  poke :: Ptr (CVector n a) -> CVector n a -> IO ()
  poke = pokeCVector

  alignment :: CVector n a -> Int
  alignment _ = sizeOf (undefined :: a)


intVal :: forall n . KnownNat n => Int
intVal = naturalToInt (natVal (Proxy @n))

peekCVector :: forall a n . (Storable a, KnownNat n) =>
  Ptr (CVector n a) -> IO (CVector n a)
peekCVector ptr = do
  let len = intVal @n
  arr <- new len
  res <- loop 0 len arr
  vector <- unsafeFreeze res
  pure (CVector vector)
  where
    loop ind len arr = do
      e <- peekByteOff ptr (ind * sizeOf (undefined :: a))
      write arr ind e
      let nextIndex = ind + 1
      if nextIndex < len then loop nextIndex len arr else pure arr

pokeCVector :: forall a n . (Storable a, KnownNat n) =>
  Ptr (CVector n a) -> CVector n a -> IO ()
pokeCVector ptr (CVector arr) = do
  let len = intVal @n
  loop 0 len
  where 
    loop ind len = do
      let e = arr ! ind
      pokeByteOff ptr (ind * sizeOf (undefined :: a)) e
      let nextIndex = ind + 1
      if nextIndex < len then loop nextIndex len else pure ()