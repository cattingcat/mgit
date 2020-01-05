{-# LANGUAGE TypeSynonymInstances #-}

module LibGit.MGitApp (
  MGitApp,
  runMGitApp
) where
import Debug.Trace (trace)

import Data.Function (on)
import Data.List
import Control.Monad.Reader

import System.Directory as Dir

import LibGit.LibGitApp

import MGit.MonadMGit
import qualified MGit.MonadGit as MG
import qualified MGit.StatusModels as S
import qualified MGit.BranchModels as B



newtype MGitAppState = MGitAppState {
  rootPath :: FilePath
}

type MGitApp = ReaderT MGitAppState IO

instance MonadMGit MGitApp where
  fetch = do
    path <- asks rootPath
    directories <- lift $ Dir.listDirectory path
    lift $ fetchAll directories
    pure ()
    where
      fetchAll dirs = runLibGitApps dirs MG.fetch

  currentBranches = do
    path <- asks rootPath
    directories <- lift $ Dir.listDirectory path
    infos <- lift $ loadAll directories
    pure $ BranchesInfo infos
    where
      loadAll dirs = runLibGitApps dirs loadRepoInfo
      loadRepoInfo = liftM2 RepoBranchInfo MG.path MG.currentBranch

  aggregateBranches cmp = do
    path <- asks rootPath
    directories <- lift $ Dir.listDirectory path
    path2Branches <- lift $ runLibGitApps directories $ do
      path <- MG.path
      branches <- MG.branches
      pure (path, branches)
    let
      maybeBranches = fmap snd path2Branches
      branches = concatMap (maybe [] B.branches) maybeBranches
      remoteBranches = filter (\case (B.RepoBranchInfo B.RemoteBranch _ _) -> True; _ -> False) branches
      remoteBranchNames = fmap B.name remoteBranches
      buckets = separateBy cmp remoteBranchNames
      aggregation = fmap (\(a:as) -> (a, length as + 1))buckets
      sorted = sortBy (compare `on` snd) aggregation
    pure $ AggregatedBranchesInfo sorted


runMGitApp :: FilePath -> MGitApp a -> IO a
runMGitApp pwd app = runReaderT app $ MGitAppState pwd

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