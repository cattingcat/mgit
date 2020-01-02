module MGit.MonadMGit where

data BranchInfo = BranchInfo {
  path :: FilePath,
  branch :: String
} deriving (Show)

newtype BranchesInfo = BranchesInfo {
  branches :: [BranchInfo]
} deriving (Show)


class Monad m => MonadMGit m where
  fetch :: m ()
  getBranches :: m BranchesInfo