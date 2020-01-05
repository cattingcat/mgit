module Foreign.GitFileStatusEnumTest where

#include <git2/status.h>

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.CStorable

newtype GitFileStatus = MkGitFileStatus CULong
    deriving (Eq)

#{enum GitFileStatus, MkGitFileStatus
    , current         = GIT_STATUS_CURRENT
	, indexNew        = GIT_STATUS_INDEX_NEW
	, indexModified   = GIT_STATUS_INDEX_MODIFIED
	, indexDeleted    = GIT_STATUS_INDEX_DELETED
	, indexRenamed    = GIT_STATUS_INDEX_RENAMED
	, indexTypeChange = GIT_STATUS_INDEX_TYPECHANGE
	, wtNew           = GIT_STATUS_WT_NEW
	, wtModified      = GIT_STATUS_WT_MODIFIED
	, wtDeleted       = GIT_STATUS_WT_DELETED
    , wtTypechange    = GIT_STATUS_WT_TYPECHANGE
	, wtRenamed       = GIT_STATUS_WT_RENAMED
	, wtUnreadable    = GIT_STATUS_WT_UNREADABLE
	, ignored         = GIT_STATUS_IGNORED
	, conflicted      = GIT_STATUS_CONFLICTED
}