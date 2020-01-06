module MGit.MonadMGit (
  RepoBranchInfo(..),
  BranchesInfo(..),
  AggregatedBranchesInfo(..),
  CheckoutSpec(..),
  MonadMGit(..),

  aggregateRemoteBranchCount,
  lookupBranches,
  checkoutSafe
) where

import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.List

import qualified MGit.BranchModels as B
import qualified MGit.RefModels as R


data RepoBranchInfo = RepoBranchInfo {
  path :: FilePath,
  branch :: Maybe B.BranchName
} deriving (Show)

newtype BranchesInfo = BranchesInfo {
  branches :: [RepoBranchInfo]
} deriving (Show)

newtype AggregatedBranchesInfo = AggregatedBranchesInfo {
  info :: [(B.BranchName, Int)]
}

newtype CheckoutSpec = CheckoutSpec {
  items :: [(FilePath, R.RefName)]
}


class Monad m => MonadMGit m where
  fetch :: m ()
  currentBranches :: m BranchesInfo
  branchesAll :: m [(FilePath, Maybe B.Branches)]
  checkout :: CheckoutSpec -> m ()


aggregateRemoteBranchCount :: MonadMGit m => (B.BranchName -> B.BranchName -> Bool) ->  m AggregatedBranchesInfo
aggregateRemoteBranchCount p = do
  path2Branches <- branchesAll
  let
    maybeBranches = fmap snd path2Branches
    branches = concatMap (maybe [] B.branches) maybeBranches
    remoteBranches = filter (\case (B.RepoBranchInfo B.RemoteBranch _ _ _) -> True; _ -> False) branches
    remoteBranchNames = fmap B.name remoteBranches
    buckets = separateBy p remoteBranchNames
    aggregation = fmap (\(a:as) -> (a, length as + 1))buckets
    sorted = sortBy (compare `on` snd) aggregation
  pure $ AggregatedBranchesInfo sorted

lookupBranches :: MonadMGit m => String -> m [(FilePath, B.Branches)]
lookupBranches searchString = do
  branches <- branchesAll
  pure $ filterEmptyRepos . filterRepos $ branches
    where
      filterEmptyRepos ::  [(FilePath, Maybe B.Branches)] -> [(FilePath, B.Branches)]
      filterEmptyRepos [] = []
      filterEmptyRepos ((p, Nothing):as) = filterEmptyRepos as
      filterEmptyRepos ((p, Just b):as) = (p, b) : filterEmptyRepos as

      filterRepos :: [(FilePath, Maybe B.Branches)] -> [(FilePath, Maybe B.Branches)]
      filterRepos = fmap $ \(p, branches) -> (p, filterBranches branches)

      filterBranches :: Maybe B.Branches -> Maybe B.Branches
      filterBranches Nothing = Nothing
      filterBranches (Just B.Branches{currentBranch, branches}) = let
        res = filter (\B.RepoBranchInfo{name} -> let (B.BranchName bName) = name in isInfixOf searchString bName) branches
        in case res of
          [] -> Nothing
          _ -> Just (B.Branches currentBranch res)

checkoutSafe :: MonadMGit m => String -> m ()
checkoutSafe searchString = do
  branchesList <- lookupBranches searchString
  let
    checkoutSpec = fmap 
      (\(path, B.Branches _ branches) -> let (B.RepoBranchInfo _ _ _ ref) = lookupSuitableBranch branches in (path, ref)) 
      branchesList

    lookupSuitableBranch :: [B.RepoBranchInfo] -> B.RepoBranchInfo
    lookupSuitableBranch bs = let
      localBranch = find (\case (B.RepoBranchInfo _ _ True _) -> True; _ -> False) bs
      branch = fromMaybe (head bs) localBranch
      in branch
  checkout (CheckoutSpec checkoutSpec)


separateBy :: (a -> a -> Bool) -> [a] -> [[a]]
separateBy _ [] = []
separateBy p as = loop p as []
  where
    loop :: (a -> a -> Bool) -> [a] -> [[a]] -> [[a]]
    loop _ [] acc = acc
    loop p (a:as) acc = loop p as (putToAcc a acc)
      where
        putToAcc a [] = [[a]]
        putToAcc a (acc:accs) = if p a (head acc) then (a:acc):accs else acc : putToAcc a accs