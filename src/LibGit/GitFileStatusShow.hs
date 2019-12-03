module LibGit.GitFileStatusShow where

import LibGit.GitFileStatus
import Data.Bits

-- | WT-prefix means changes which wasn't added to index
-- | no prefix - changes from index
instance Show GitFileStatus where
  show a
         | a `hasFlag` indexRenamed    = "renamed"
         | a `hasFlag` wtRenamed       = "wt renamed"
         | a `hasFlag` indexModified   = "modified"
         | a `hasFlag` indexDeleted    = "deleted"
         | a `hasFlag` wtModified      = "wt modified"
         | a `hasFlag` wtDeleted       = "wt deleted"
         | a `hasFlag` indexTypeChange = "type change"
         | a `hasFlag` wtTypechange    = "wt type change"
         | a `hasFlag` wtUnreadable    = "wt undeadable"
         | a `hasFlag` indexNew        = "new"
         | a `hasFlag` wtNew           = "wt new"
         | a `hasFlag` ignored         = ""
         | a `hasFlag` conflicted      = "conflict"
         | otherwise            = "unknown"
         --  | a `hasFlag` current         = ""


hasFlag :: GitFileStatus -> GitFileStatus -> Bool
hasFlag (MkGitFileStatus v) (MkGitFileStatus f) = v .&. f == f