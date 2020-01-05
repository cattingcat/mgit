module MGit.MonadMGit (
  RepoBranchInfo(..),
  BranchesInfo(..),
  AggregatedBranchesInfo(..),
  MonadMGit(..),

  aggregateRemoteBranchCount
) where

import Data.Function (on)
import Data.List

import qualified MGit.BranchModels as B


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


class Monad m => MonadMGit m where
  fetch :: m ()
  currentBranches :: m BranchesInfo
  branchesAll :: m [(FilePath, Maybe B.Branches)]


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