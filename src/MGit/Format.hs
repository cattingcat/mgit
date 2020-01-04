module MGit.Format where
import MGit.MonadMGit
import Data.Maybe (fromMaybe)
import System.FilePath.Posix (makeRelative)

formatBranchesInfo :: FilePath -> BranchesInfo -> [String]
formatBranchesInfo pwd (BranchesInfo branches) = fmap formatLine branches
  where
    formatLine (BranchInfo path branch) =
      let relativePath = makeRelative pwd path
          len = length relativePath
          spaceLen = 40 - len
          space = " " <> (if spaceLen > 0 then replicate spaceLen ' ' else "") <> " "
       in relativePath <> space <> fromMaybe "no branches" branch

printBranchesInfo :: FilePath -> BranchesInfo -> IO ()
printBranchesInfo pwd info = do
  let lines = formatBranchesInfo pwd info
  mapM_ putStrLn lines