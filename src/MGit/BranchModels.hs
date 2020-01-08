module MGit.BranchModels where

import qualified MGit.RefModels as R


newtype BranchName = BranchName String
  deriving stock (Show, Eq)

data BranchType = RemoteBranch | LocalBranch
  deriving stock (Show)

data RepoBranchInfo = RepoBranchInfo {
  branchType :: BranchType,
  name :: BranchName,
  isBranch :: Bool,
  ref :: R.RefName
} deriving stock (Show)

data Branches = Branches {
  currentBranch :: RepoBranchInfo,
  branches :: [RepoBranchInfo]
} deriving stock (Show)