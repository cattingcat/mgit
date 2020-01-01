{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Foreign.CVector where

import GHC.Base
import GHC.TypeNats
import GHC.Natural

import Data.Data
import Data.Vector
import Data.Vector.Mutable
import qualified Data.Vector.Generic as G

import Foreign.Ptr
import Foreign.Storable


newtype CVector (n :: Nat) (a :: Type) = CVector (Vector a)

instance (Storable a, KnownNat n) => Storable (CVector n a) where
  sizeOf :: CVector n a -> Int
  sizeOf arr = intVal @n * sizeOf (undefined :: a)

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
  res <- loop 0 len ptr arr
  vector <- unsafeFreeze res
  pure (CVector vector)
  where
    loop :: forall a b . Storable b => Int -> Int -> Ptr a -> IOVector b -> IO (IOVector b)
    loop i n ptr arr = do
      e <- peekByteOff ptr (i * sizeOf (undefined :: b))
      write arr i e
      let nextIndex = i + 1
      if nextIndex < n then loop nextIndex n ptr arr else pure arr

pokeCVector :: forall a n . (Storable a, KnownNat n) =>
  Ptr (CVector n a) -> CVector n a -> IO ()
pokeCVector ptr (CVector arr) = do
  let len = intVal @n
  loop 0 len ptr arr 
  where 
    loop :: forall a b . Storable b => Int -> Int -> Ptr a -> Vector b -> IO ()
    loop i n ptr arr = do
      let e = arr ! i
      pokeByteOff ptr (i * sizeOf (undefined :: b)) e
      let nextIndex = i + 1
      if nextIndex < n then loop nextIndex n ptr arr else pure ()