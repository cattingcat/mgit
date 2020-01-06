module MGit.RefModels where

newtype RefName = RefName String
  deriving (Show)

data RefType = Remote | Head | Tag
  deriving (Show)

data RefInfo = RefInfo {
  refType :: RefType,
  name :: RefName,
  lastCommitMsg :: String
} deriving (Show)
