{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module LibGit.Models where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable
import Foreign.CStorableWrap
import Control.Exception (Exception, throw)
import GHC.Generics (Generic)
import Data.Data (Typeable)


data GitRepo = GitRepo
  deriving (Generic, CStorable)

type GitRepoPtr = Ptr GitRepo

-- git_reference
data GitReference = GitReference
  deriving (Generic, CStorable)

newtype OpenRepoError = OpenRepoError CInt deriving (Show, Typeable)
instance Exception OpenRepoError

data GitStrArr = GitStrArr {
  strings :: !(Ptr CString),
  count :: !CSize
}
  deriving (Generic, CStorable)
  deriving (Storable) via (CStorableWrapper GitStrArr)

makeStrArr :: [String] -> IO (Ptr GitStrArr)
makeStrArr strings = do
  ptrs <- mapM newCString strings
  let len = length strings
  arr <- newArray ptrs
  ptr <- malloc
  poke ptr $ GitStrArr arr (toEnum len)
  pure ptr

freeStrArr :: Ptr GitStrArr -> IO ()
freeStrArr ptr = do
  arr <- peek ptr
  loop (strings arr) (fromIntegral . count $ arr) 0
  free ptr
  where
    loop arr count index = do
      cstr <- peekElemOff arr index
      free cstr
      let newIndex = index + 1
      if newIndex < count
        then loop arr count newIndex
        else pure ()