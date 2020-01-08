module MGit.RefModels where

newtype RefName = RefName String
  deriving stock (Show)

data RefType = Remote | Head | Tag
  deriving stock (Show)

data RefInfo = RefInfo {
  refType :: RefType,
  name :: RefName,
  lastCommitMsg :: String
} deriving stock (Show)
