{-# LANGUAGE AllowAmbiguousTypes #-}

module Foreign.CArray where

import GHC.Base
import GHC.TypeNats
import GHC.Natural

import Data.Data
import Data.Array.Base
import Data.Array.IO

import Foreign.Ptr
import Foreign.Storable


newtype CArrayInternal (n :: Nat) i (a :: Type) = CArray (IOArray i a)

  
type CArray (n :: Nat) (a :: Type) = CArrayInternal n Int a

instance (Storable a, KnownNat n) => Storable (CArrayInternal n Int a) where
  sizeOf :: CArray n a -> Int
  sizeOf _ = intVal @n * sizeOf (undefined :: a)

  peek :: Ptr (CArray n a) -> IO (CArray n a)
  peek = peekCArray

  poke :: Ptr (CArray n a) -> CArray n a -> IO ()
  poke = pokeCArray

  alignment :: CArray n a -> Int
  alignment _ = sizeOf (undefined :: a)


intVal :: forall n . KnownNat n => Int
intVal = naturalToInt (natVal (Proxy @n))

peekCArray :: forall a n . (Storable a, KnownNat n) => 
  Ptr (CArray n a) -> IO (CArray n a)
peekCArray ptr = do
  let len = intVal @n
  arr <- newArray_ (0, len - 1)
  res <- loop 0 len ptr arr
  pure (CArray res)
  where
    loop ind len p arr = do
      e <- peekByteOff p (ind * sizeOf (undefined :: a))
      writeArray arr ind e
      let nextIndex = ind + 1
      if nextIndex < len then loop nextIndex len p arr else pure arr

pokeCArray :: forall a n . (Storable a, KnownNat n) => 
  Ptr (CArray n a) -> CArray n a -> IO ()
pokeCArray ptr (CArray arr) = do
  let len = intVal @n
  loop 0 len
  where 
    loop ind len = do
      e <- readArray arr ind
      pokeByteOff ptr (ind * sizeOf (undefined :: a)) e
      let nextIndex = ind + 1
      if nextIndex < len then loop nextIndex len else pure ()