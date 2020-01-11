{-# LANGUAGE UndecidableInstances #-}

module LibGit.Branch (
  BranchType(..),
  RepoBranchInfo(..),
  Branches(..),

  localBranch,
  remoteBranch,
  allBranches,

  getBranches,
  createBranchFromRemote
) where

import System.IO (IO)
import Control.Applicative (pure)
import Control.Category ((.))
import Data.Maybe (Maybe(..))
import Data.Eq ((==))
import Data.Ord ((<))
import Data.Function (($))
import Data.Bool (otherwise)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Text.Show (show)
import GHC.Err (error)

import MGit.BranchModels
import MGit.RefModels

import LibGit.Refs as R
import LibGit.AnnotatedCommit as A

import Foreign
import Foreign.C.Types
import Foreign.C.String (peekCString)
import Foreign.LibGit.Models
import Foreign.LibGit.Branch




localBranch :: GitBranchType
localBranch = GitBranchType 1

remoteBranch :: GitBranchType
remoteBranch = GitBranchType 2

allBranches :: GitBranchType
allBranches = GitBranchType $ (1 :: CUChar) .|. (2 :: CUChar)

-- GIT_ITEROVER
iterOver :: CInt
iterOver = -31


getBranches :: GitRepoPtr -> IO (Maybe Branches)
getBranches repoPtr = do
  iterPP <- malloc
  strPtr <- malloc
  referencePP <- malloc
  branchTypePtr <- malloc
  _ <- c_git_branch_iterator_new iterPP repoPtr allBranches
  -- todo: ^ check res
  iterPtr <- peek iterPP
  (head, bs) <- loop referencePP branchTypePtr iterPtr strPtr (Nothing, [])
  free strPtr
  free referencePP
  free branchTypePtr
  c_git_branch_iterator_free iterPtr
  free iterPP
  case head of
    Nothing -> pure Nothing
    Just h -> pure $ Just $ Branches h bs
  where
    loop rpp branchTypePtr iterPtr branchNamePtr accum = do
      nextRes <- c_git_branch_next rpp branchTypePtr iterPtr
      if
        | nextRes == iterOver -> pure accum
        | nextRes < 0         -> error $ "getBranches iter error no: " <> show nextRes
        | otherwise           -> processNextIteration accum
      where
        processNextIteration (headBranch, branches) = do
          referencePtr <- peek rpp
          c_git_branch_name branchNamePtr referencePtr
          str <- peek branchNamePtr
          isHeadRes <- c_git_branch_is_head referencePtr
          isBranchRef <- R.isBranch referencePtr
          branchName <- peekCString str
          branchType <- peek branchTypePtr
          referenceName <- R.refName referencePtr
          let
            isHead = isHeadRes == 1
            bType = if branchType == localBranch
              then LocalBranch
              else RemoteBranch
            branchInfo = RepoBranchInfo bType (BranchName . T.pack $ branchName) isBranchRef (RefName referenceName)
            head = case headBranch of
              Nothing -> if isHead then Just branchInfo else headBranch
              Just _ -> headBranch
            tpl = (head, branchInfo:branches)

          loop rpp branchTypePtr iterPtr branchNamePtr tpl


createBranchFromRemote :: GitRepoPtr -> T.Text -> Ptr A.GitAnnotatedCommit -> IO GitRefPtr
createBranchFromRemote repo branchName commit = T.withCStringLen branchName $ \(s, _) -> do
  p <- malloc
  _ <- c_git_branch_create_from_annotated p repo s commit 0
  res <- peek p
  free p
  pure res