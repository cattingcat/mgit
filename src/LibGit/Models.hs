{-# LANGUAGE UndecidableInstances #-}

module LibGit.Models where

import Data.Data (Typeable)
import Control.Exception (Exception)

import GHC.Generics (Generic)

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable
import Foreign.CStorableWrap


data GitOid = GitOid
  deriving stock (Generic)
  deriving anyclass (CStorable)

data GitRepo = GitRepo
  deriving stock (Generic)
  deriving anyclass (CStorable)

type GitRepoPtr = Ptr GitRepo

-- git_reference
data GitReference = GitReference
  deriving stock (Generic)
  deriving anyclass (CStorable)
  
type GitRefPtr = Ptr GitReference

newtype OpenRepoError = OpenRepoError CInt 
  deriving stock (Show, Typeable)
instance Exception OpenRepoError

data GitStrArr = GitStrArr {
  strings :: !(Ptr CString),
  count :: !CSize
}
  deriving stock (Generic)
  deriving anyclass (CStorable)
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