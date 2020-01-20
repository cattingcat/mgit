module MGit.MonadGit (
  MonadGit(..),
  
  currentBranch,
  lookupBranches,
  checkoutSafe
) where

import System.FilePath
import Control.Monad
import Control.Applicative
import qualified Data.List as L
import Data.Bool
import Data.Maybe
import Data.Monoid
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

lookupBranches :: MonadGit m => Text -> m [B.RepoBranchInfo]
lookupBranches search = do
  r <- branches
  case r of
    Nothing -> pure []
    Just repoBranches -> do
      let
        (B.Branches _ bs) = repoBranches
        filtered = L.filter (\(B.RepoBranchInfo _ (B.BranchName name) _ _) -> search `isInfixOf` name)  bs
      pure filtered

checkoutSafe :: MonadGit m => Text -> m (Maybe R.RefInfo)
checkoutSafe branchName = do
  bs <- lookupBranches branchName
  let
    (remotes, locals) = L.span (\case (B.RepoBranchInfo B.RemoteBranch _ _ _) -> True; _ -> False) bs
  case locals <> remotes of
    [] -> pure Nothing
    (B.RepoBranchInfo _ _ _ n@(R.RefName refName) : _) -> lookupRef refName <* checkoutTree n
