{-# LANGUAGE UndecidableInstances #-}

module Foreign.LibGit.Models where

import System.IO (IO)
import Data.Ord
import Data.List as L
import Data.Text hiding (count)
import Data.Function (($))
import Control.Applicative 
import Control.Monad
import Control.Category

import GHC.Enum (toEnum)
import GHC.Generics (Generic)
import GHC.Num ((+))
import GHC.Real (fromIntegral)

import Foreign
import Foreign.C.Types
import Foreign.C.String (CString, newCString)
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

data GitCommit = GitCommit
  deriving stock (Generic)
  deriving anyclass (CStorable)




data GitStrArr = GitStrArr {
  strings :: !(Ptr CString),
  count :: !CSize
}
  deriving stock (Generic)
  deriving anyclass (CStorable)
  deriving (Storable) via (CStorableWrapper GitStrArr)

makeStrArr :: [Text] -> IO (Ptr GitStrArr)
makeStrArr strings = do
  ptrs <- mapM (newCString . unpack) strings
  let len = L.length strings
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
    loop arr count ind = do
      cstr <- peekElemOff arr ind
      free cstr
      let newIndex = ind + 1
      if newIndex < count
        then loop arr count newIndex
        else pure ()