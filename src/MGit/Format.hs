module MGit.Format where
import qualified MGit.BranchModels as B
import MGit.MonadMGit
import Data.Maybe (fromMaybe)
import System.FilePath.Posix (makeRelative)

formatBranchesInfo :: FilePath -> BranchesInfo -> [String]
formatBranchesInfo pwd (BranchesInfo branches) = fmap formatLine branches
  where
    formatLine (RepoBranchInfo path branch) =
      let relativePath = makeRelative pwd path
          len = length relativePath
          spaceLen = 40 - len
          space = " " <> (if spaceLen > 0 then replicate spaceLen ' ' else "") <> " "
       in relativePath <> space <> maybe "no branches" (\(B.BranchName n) -> n) branch

printBranchesInfo :: FilePath -> BranchesInfo -> IO ()
printBranchesInfo pwd info = do
  let lines = formatBranchesInfo pwd info
  mapM_ putStrLn lines
  
formatBranchAggregationInfo :: AggregatedBranchesInfo -> [String]
formatBranchAggregationInfo (AggregatedBranchesInfo infos) = fmap formatLine infos
  where 
    formatLine (B.BranchName name, count) = 
      let len = length name
          spaceLen = 40 - len
          space = " " <> (if spaceLen > 0 then replicate spaceLen ' ' else "") <> ""
       in name <> space <> show count 
       
printBranchAggregationInfo :: AggregatedBranchesInfo -> IO ()
printBranchAggregationInfo info = do
  let lines = formatBranchAggregationInfo info
  mapM_ putStrLn lines