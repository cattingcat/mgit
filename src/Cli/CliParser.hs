module Cli.CliParser where

import System.Environment

data CliCommand =
    Branch
  | Branches
  | Fetch
  | Test
  | NoCommand
  | ErrorCommand
  deriving (Show)

parseCli :: IO CliCommand
parseCli = do
  cliArgs <- getArgs
  case cliArgs of
    []    -> pure NoCommand
    (h:t) -> if | h == "branch"   -> pure (parseBranch t)
                | h == "branches"   -> pure (parseBranches t)
                | h == "fetch"    -> pure (parseFetch t)
                | h == "test"     -> pure (parseTest t)
                | otherwise       -> pure ErrorCommand
                
  where 
    parseBranch args = Branch
    parseBranches args = Branches
    parseFetch args = Fetch
    parseTest args = Test