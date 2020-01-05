{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UndecidableInstances #-}

module LibGit.Branch (
  BranchType(..),
  RepoBranchInfo(..),
  Branches(..),
  getBranches
) where

import System.Directory
import MGit.BranchModels
import LibGit.Models
import LibGit.Refs as R
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable
import Foreign.Storable
import GHC.Generics (Generic)
import Foreign.CStorableWrap

-- git_branch_iterator
data GitBranchIterator = GitBranchIterator
  deriving (Generic, CStorable)

-- git_branch_t
newtype GitBranchType = GitBranchType CUChar
  deriving (Eq, Show, Generic, CStorable)
  deriving Storable via (CStorableWrapper GitBranchType)


localBranch :: GitBranchType
localBranch = GitBranchType 1

remoteBranch :: GitBranchType
remoteBranch = GitBranchType 2

allBranches :: GitBranchType
allBranches = GitBranchType $ (1 :: CUChar) .|. (2 :: CUChar)

-- GIT_ITEROVER
iterOver :: CInt
iterOver = -31

-- int git_branch_iterator_new(git_branch_iterator **out, git_repository *repo, git_branch_t list_flags);
foreign import ccall "git2/branch.h git_branch_iterator_new" c_git_branch_iterator_new :: Ptr (Ptr GitBranchIterator) -> Ptr GitRepo -> GitBranchType -> IO CInt

-- void git_branch_iterator_free(git_branch_iterator *iter);
foreign import ccall "git2/branch.h git_branch_iterator_free" c_git_branch_iterator_free :: Ptr GitBranchIterator -> IO ()

-- int git_branch_next(git_reference **out, git_branch_t *out_type, git_branch_iterator *iter);
foreign import ccall "git2/branch.h git_branch_next" c_git_branch_next :: Ptr (Ptr GitReference) -> Ptr GitBranchType -> Ptr GitBranchIterator -> IO CInt

-- int git_branch_name(const char **out, const git_reference *ref);
foreign import ccall "git2/branch.h git_branch_name" c_git_branch_name :: Ptr CString -> Ptr GitReference -> IO CInt

-- int git_branch_is_head(const git_reference *branch);
foreign import ccall "git2/branch.h git_branch_is_head" c_git_branch_is_head :: Ptr GitReference -> IO CInt



getBranches :: GitRepoPtr -> IO (Maybe Branches)
getBranches repoPtr = do
  iterPP <- malloc
  strPtr <- malloc
  referencePP <- malloc
  branchTypePtr <- malloc
  r <- c_git_branch_iterator_new iterPP repoPtr allBranches
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
        | otherwise           -> processNextIter rpp branchTypePtr iterPtr branchNamePtr accum
      where
        processNextIter rpp branchTypePtr iterPtr branchNamePtr acc@(headBranch, branches) = do
          referencePtr <- peek rpp
          c_git_branch_name branchNamePtr referencePtr
          str <- peek branchNamePtr
          isHeadRes <- c_git_branch_is_head referencePtr
          isBranchRef <- R.isBranch referencePtr
          branchName <- peekCString str
          branchType <- peek branchTypePtr
          let
            isHead = isHeadRes == 1
            bType = if branchType == localBranch
              then LocalBranch
              else RemoteBranch
            branchInfo = RepoBranchInfo bType (BranchName branchName) isBranchRef
            head = case headBranch of
              Nothing -> if isHead then Just branchInfo else headBranch
              Just _ -> headBranch
            tpl = (head, branchInfo:branches)

          loop rpp branchTypePtr iterPtr branchNamePtr tpl