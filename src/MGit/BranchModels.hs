module MGit.BranchModels where

data BranchType = RemoteBranch | LocalBranch
  deriving (Show)

data BranchInfo = BranchInfo {
  branchType :: BranchType,
  name :: String,
  isBranch :: Bool
} deriving (Show)

data Branches = Branches {
  currentBranch :: BranchInfo,
  branches :: [BranchInfo]
} deriving (Show)