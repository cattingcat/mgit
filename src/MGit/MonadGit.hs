module MGit.MonadGit where

import Prelude ()

import System.FilePath
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Function
import Data.Text

import qualified MGit.StatusModels as S
import qualified MGit.BranchModels as B
import qualified MGit.RefModels as R


class Monad m => MonadGit m where
  fetch :: m ()
  branches :: m (Maybe B.Branches)
  status :: m S.StatusInfo
  path :: m FilePath
  lookupRef :: Text -> m (Maybe R.RefInfo)
  checkoutTree :: R.RefName -> m ()


currentBranch :: MonadGit m => m (Maybe B.BranchName)
currentBranch = do
  branchesRes <- branches
  pure $ B.name . B.currentBranch <$>  branchesRes
  