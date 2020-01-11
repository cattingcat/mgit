module MGit.Format (
  printBranchesInfo,
  printBranchAggregationInfo,
  printBranchesLookup,
  printRepos
) where

import System.IO (IO)
import System.FilePath (FilePath)
import System.FilePath.Posix (makeRelative)
import Control.Monad (mapM_)
import Control.Applicative ((*>))

import Control.Category ((.))
import Data.Tuple (snd, fst)
import Data.Maybe (Maybe(..))
import Data.Text.IO (putStrLn)
import Data.Function (($))
import Data.Text
import Text.Show (show)

import qualified MGit.MonadMGit as MG
import qualified MGit.BranchModels as B
import qualified MGit.RefModels as R

import PrintTable.Print


printBranchesInfo :: FilePath -> MG.BranchesInfo -> IO ()
printBranchesInfo pwd (MG.BranchesInfo branches) = let
  accessPath = pack . makeRelative pwd . MG.path
  accessName = formatBranchMaybe . MG.branch
  formatBranchMaybe Nothing = "no branches"
  formatBranchMaybe (Just (B.BranchName name)) = name
  in printTable @('MaxLen :|: 'MaxLen :|: Endl) (C accessPath :| C accessName :| Endl) branches

printBranchAggregationInfo :: MG.AggregatedBranchesInfo -> IO ()
printBranchAggregationInfo (MG.AggregatedBranchesInfo infos) = let
  branchName (B.BranchName name, _) = name
  getCount = pack . show . snd
  in printTable @('MaxLen :|: 'MaxLen :|: Endl) (C branchName :| C getCount :| Endl) infos

printBranchesLookup :: FilePath -> [(FilePath, B.Branches)] -> IO ()
printBranchesLookup path ms = let
  getPath = pack . makeRelative path . fst
  getBranchName (B.RepoBranchInfo _ (B.BranchName name) _ _) = name
  getBranchRef  (B.RepoBranchInfo _ _ _ (R.RefName ref)) = ref
  printBranches = printTable @('MaxLen :|: 'MaxLen :|: Endl) (C getBranchName :| C getBranchRef :| Endl)
  printPath i = putStrLn $ getPath i
  in mapM_ (\i -> (printPath i) *> (printBranches . B.branches . snd $ i) *> putStrLn "") ms

printRepos :: [FilePath] -> IO ()
printRepos = printTable @('MaxLen :|: Endl) (C pack :| Endl)