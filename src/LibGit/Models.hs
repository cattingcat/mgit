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
import LibGit.GitStatus
import FFI.Storable
import GHC.Generics (Generic)


data GitRepo = GitRepo
  deriving (Generic, CStorable)

data GitStrArr = GitStrArr {
  strings :: !(Ptr CString),
  count :: !CSize
}
  deriving (Generic, CStorable)
  deriving (Storable) via (CStorableWrapper GitStrArr)

makeStrArr :: [String] -> IO (Ptr GitStrArr)
makeStrArr strs = do
  ptrs <- mapM newCString strs
  let len = length strs
  arr <- newArray ptrs
  ptr <- malloc
  poke ptr $ GitStrArr arr (toEnum len)
  pure ptr
