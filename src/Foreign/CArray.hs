{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Foreign.CArray where

import GHC.Base
import GHC.TypeNats
import GHC.Natural

import Data.Data
import Data.Array.Base
import Data.Array.ST
import Data.Array.IO
import Data.Array.MArray

import Foreign.Ptr
import Foreign.Storable


newtype CArrayInternal (n :: Nat) i (a :: Type) = CArray (IOArray i a)

  
type CArray (n :: Nat) (a :: Type) = CArrayInternal n Int a

instance (Storable a, KnownNat n) => Storable (CArrayInternal n Int a) where
  sizeOf :: CArray n a -> Int
  sizeOf arr = intVal @n * sizeOf (undefined :: a)

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
    loop :: forall a b . Storable b => Int -> Int -> Ptr a -> IOArray Int b -> IO (IOArray Int b)
    loop i n ptr arr = do
      e <- peekByteOff ptr (i * sizeOf (undefined :: b))
      writeArray arr i e
      let nextIndex = i + 1
      if nextIndex < n then loop nextIndex n ptr arr else pure arr

pokeCArray :: forall a n . (Storable a, KnownNat n) => 
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