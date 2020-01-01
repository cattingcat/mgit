module MGit.MonadGit where
import qualified MGit.StatusModels as S
import qualified MGit.BranchModels as B

class Monad m => MonadGit m where
  fetch :: m ()
  branches :: m B.Branches
  status :: m S.StatusInfo

currentBranch :: MonadGit m => m String
currentBranch = do
  branchesRes <- branches
  pure . B.name . B.currentBranch $ branchesRes