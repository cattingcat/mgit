module LibGit.GitStatus where

#include <git2/status.h>

import Foreign.C.Types

-- git_status_t
newtype GitStatus = MkGitStatus CULong
    deriving stock (Eq)