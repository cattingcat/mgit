{-# LANGUAGE BangPatterns #-}

module LibGit.GitStatus where

#include <git2/status.h>

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable

-- git_status_t
newtype GitStatus = MkGitStatus CULong
    deriving (Eq)