module MGit.MonadMGit where
import qualified MGit.BranchModels as B

data RepoBranchInfo = RepoBranchInfo {
  path :: FilePath,
  branch :: Maybe B.BranchName
} deriving (Show)

newtype BranchesInfo = BranchesInfo {
  branches :: [RepoBranchInfo]
} deriving (Show)

data AggregatedBranchesInfo = AggregatedBranchesInfo {
  info :: [(B.BranchName, Int)]
}


class Monad m => MonadMGit m where
  fetch :: m ()
  currentBranches :: m BranchesInfo
  aggregateBranches :: (B.BranchName -> B.BranchName -> Bool) ->  m AggregatedBranchesInfo