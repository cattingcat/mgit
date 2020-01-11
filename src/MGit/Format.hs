module MGit.Format (
  printBranchesInfo,
  printBranchAggregationInfo,
  printBranchesLookup,
  printRepos
) where

import System.IO (IO)
import System.FilePath (FilePath)
import System.FilePath.Posix (makeRelative)

import Control.Category ((.))
import Control.Monad (mapM_)
import Data.Monoid ((<>))
import Data.Ord ((>))
import Data.Functor (fmap)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Show (show)

import GHC.Num ((-))

import qualified MGit.MonadMGit as MG
import qualified MGit.BranchModels as B

import PrintTable.Print


printBranchesInfo :: FilePath -> MG.BranchesInfo -> IO ()
printBranchesInfo pwd (MG.BranchesInfo branches) = let
  accessPath = T.pack . makeRelative pwd . MG.path
  accessName = formatBranchMaybe . MG.branch
  formatBranchMaybe Nothing = "no branches"
  formatBranchMaybe (Just (B.BranchName name)) = name
  in printTable @('MaxLen :|: 'MaxLen :|: Endl) (C accessPath :| C accessName :| Endl) branches

  
formatBranchAggregationInfo :: MG.AggregatedBranchesInfo -> [T.Text]
formatBranchAggregationInfo (MG.AggregatedBranchesInfo infos) = fmap formatLine infos
  where 
    formatLine (B.BranchName name, count) = 
      let len = T.length name
          spaceLen = 40 - len
          space = " " <> (if spaceLen > 0 then T.replicate spaceLen " " else "") <> ""
       in name <> space <> T.pack (show count)
       
printBranchAggregationInfo :: MG.AggregatedBranchesInfo -> IO ()
printBranchAggregationInfo info = do
  let lines = formatBranchAggregationInfo info
  mapM_ T.putStrLn lines

formatBranchesLookup :: FilePath -> [(FilePath, B.Branches)] -> [T.Text]
formatBranchesLookup _ [] = []
formatBranchesLookup pwd ((p, b):as) = (T.pack $ makeRelative pwd p) : prepareBranches b <> formatBranchesLookup pwd as
  where
    prepareBranches B.Branches{branches} = fmap prepareBranchInfo branches
    prepareBranchInfo (B.RepoBranchInfo _ (B.BranchName name) _ ref) = let
      len = T.length name
      spaceLen = 60 - len
      space = " " <> (if spaceLen > 0 then T.replicate spaceLen "  " else "") <> ""
      in "  " <> name <> space <> T.pack(show ref)

printBranchesLookup :: FilePath -> [(FilePath, B.Branches)] -> IO ()
printBranchesLookup pwd d = do
  let lines = formatBranchesLookup pwd d
  mapM_ T.putStrLn lines
  
printRepos :: [FilePath] -> IO ()
printRepos = printTable @('MaxLen :|: Endl) (C T.pack :| Endl)