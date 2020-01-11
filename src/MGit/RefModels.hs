module MGit.RefModels where

import Prelude ()

import Data.Text
import Text.Show (Show)


newtype RefName = RefName Text
  deriving stock (Show)

data RefType = Remote | Head | Tag
  deriving stock (Show)

data RefInfo = RefInfo {
  refType :: RefType,
  name :: RefName,
  lastCommitMsg :: Text
} deriving stock (Show)
