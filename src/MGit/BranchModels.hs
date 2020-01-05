module MGit.BranchModels where

newtype BranchName = BranchName String
  deriving (Show, Eq)

data BranchType = RemoteBranch | LocalBranch
  deriving (Show)

data RepoBranchInfo = RepoBranchInfo {
  branchType :: BranchType,
  name :: BranchName,
  isBranch :: Bool
} deriving (Show)

data Branches = Branches {
  currentBranch :: RepoBranchInfo,
  branches :: [RepoBranchInfo]
} deriving (Show)