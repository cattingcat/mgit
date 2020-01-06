module MGit.BranchModels where

import qualified MGit.RefModels as R


newtype BranchName = BranchName String
  deriving (Show, Eq)

data BranchType = RemoteBranch | LocalBranch
  deriving (Show)

data RepoBranchInfo = RepoBranchInfo {
  branchType :: BranchType,
  name :: BranchName,
  isBranch :: Bool,
  ref :: R.RefName
} deriving (Show)

data Branches = Branches {
  currentBranch :: RepoBranchInfo,
  branches :: [RepoBranchInfo]
} deriving (Show)