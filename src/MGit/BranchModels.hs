module MGit.BranchModels(
  BranchName(..),
  BranchType(..),
  RepoBranchInfo(..),
  Branches(..)
) where

import Data.Eq
import Data.Bool
import Data.Text

import GHC.Show

import MGit.RefModels qualified as R


newtype BranchName = BranchName Text
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