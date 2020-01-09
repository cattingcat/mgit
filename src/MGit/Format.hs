module MGit.Format (
  printBranchesInfo,
  printBranchAggregationInfo,
  printBranchesLookup
) where

import Prelude hiding (lines)

import System.FilePath.Posix (makeRelative)

import qualified MGit.MonadMGit as MG
import qualified MGit.BranchModels as B
import PrintTable.Print


printBranchesInfo :: FilePath -> MG.BranchesInfo -> IO ()
printBranchesInfo pwd (MG.BranchesInfo branches) = let
  accessPath = makeRelative pwd . MG.path
  accessName = formatBranchMaybe . MG.branch
  formatBranchMaybe Nothing = "no branches"
  formatBranchMaybe (Just (B.BranchName name)) = name
  in printTable @('MaxLen :|: 'MaxLen :|: Endl) (C accessPath :| C accessName :| Endl) branches

  
formatBranchAggregationInfo :: MG.AggregatedBranchesInfo -> [String]
formatBranchAggregationInfo (MG.AggregatedBranchesInfo infos) = fmap formatLine infos
  where 
    formatLine (B.BranchName name, count) = 
      let len = length name
          spaceLen = 40 - len
          space = " " <> (if spaceLen > 0 then replicate spaceLen ' ' else "") <> ""
       in name <> space <> show count 
       
printBranchAggregationInfo :: MG.AggregatedBranchesInfo -> IO ()
printBranchAggregationInfo info = do
  let lines = formatBranchAggregationInfo info
  mapM_ putStrLn lines

formatBranchesLookup :: FilePath -> [(FilePath, B.Branches)] -> [String]
formatBranchesLookup _ [] = []
formatBranchesLookup pwd ((p, b):as) = (makeRelative pwd p) : prepareBranches b <> formatBranchesLookup pwd as
  where
    prepareBranches B.Branches{branches} = fmap prepareBranchInfo branches
    prepareBranchInfo (B.RepoBranchInfo _ (B.BranchName name) _ ref) = let
      len = length name
      spaceLen = 60 - len
      space = " " <> (if spaceLen > 0 then replicate spaceLen ' ' else "") <> ""
      in "  " <> name <> space <> show ref

printBranchesLookup :: FilePath -> [(FilePath, B.Branches)] -> IO ()
printBranchesLookup pwd d = do
  let lines = formatBranchesLookup pwd d
  mapM_ putStrLn lines