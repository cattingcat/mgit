{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

module FFI.CArray where

import GHC.Base
import qualified GHC.TypeNats as TL
import Data.Array.ST
import Data.Array.Base
import Foreign.Storable
import Data.Data
import GHC.Natural
import Foreign.Ptr
import Data.Array.IO


newtype CArray (n :: Nat) (a :: Type) = CArray (IOArray Int a)

instance (Storable a, TL.KnownNat n) => Storable (CArray n a) where
  sizeOf :: CArray n a -> Int
  sizeOf arr = intVal @n * sizeOf (undefined :: a)

  peek :: Ptr (CArray n a) -> IO (CArray n a)
  peek = peekCArray

  poke :: Ptr (CArray n a) -> CArray n a -> IO ()
  poke = pokeCArray

  alignment :: CArray n a -> Int
  alignment _ = sizeOf (undefined :: a)


intVal :: forall n . TL.KnownNat n => Int
intVal = naturalToInt (TL.natVal (Proxy @n))

peekCArray :: forall a n . (Storable a, TL.KnownNat n) => 
  Ptr (CArray n a) -> IO (CArray n a)
peekCArray ptr = do
  let len = intVal @n
  arr <- newArray_ (0, len - 1)
  res <- loop 0 len ptr arr
  pure (CArray res)
  where
    loop :: forall a b . Storable b => Int -> Int -> Ptr a -> IOArray Int b -> IO (IOArray Int b)
    loop i n ptr arr = do
      e <- peekByteOff ptr (i * sizeOf (undefined :: b))
      writeArray arr i e
      let nextIndex = i + 1
      if nextIndex < n then loop nextIndex n ptr arr else pure arr

pokeCArray :: forall a n . (Storable a, TL.KnownNat n) => 
  Ptr (CArray n a) -> CArray n a -> IO ()
pokeCArray ptr (CArray arr) = do
  let len = intVal @n
  loop 0 len ptr arr 
  where 
    loop :: forall a b . Storable b => Int -> Int -> Ptr a -> IOArray Int b -> IO ()
    loop i n ptr arr = do
      e <- readArray arr i
      pokeByteOff ptr (i * sizeOf (undefined :: b)) e
      let nextIndex = i + 1
      if nextIndex < n then loop nextIndex n ptr arr else pure ()