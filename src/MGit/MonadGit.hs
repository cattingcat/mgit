module MGit.MonadGit where
import qualified MGit.StatusModels as S
import qualified MGit.BranchModels as B

class Monad m => MonadGit m where
  fetch :: m ()
  branches :: m (Maybe B.Branches)
  status :: m S.StatusInfo
  path :: m FilePath

currentBranch :: MonadGit m => m (Maybe String)
currentBranch = do
  branchesRes <- branches
  pure $ B.name . B.currentBranch <$>  branchesRes