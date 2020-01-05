module MGit.BranchModels where

newtype BranchName = BranchName String
  deriving (Show, Eq)

newtype RefName = RefName String
  deriving (Show)

data BranchType = RemoteBranch | LocalBranch
  deriving (Show)

data RepoBranchInfo = RepoBranchInfo {
  branchType :: BranchType,
  name :: BranchName,
  isBranch :: Bool,
  ref :: RefName
} deriving (Show)

data Branches = Branches {
  currentBranch :: RepoBranchInfo,
  branches :: [RepoBranchInfo]
} deriving (Show)