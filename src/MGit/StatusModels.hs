module MGit.StatusModels(
  DeltaStatus(..),
  DeltaInfo(..),
  StatusEntryDeltaInfo(..),
  StatusInfo(..)
) where

import System.FilePath
import Data.Maybe
import Text.Show


data DeltaStatus =
    Unmodified
  | Added
  | Deleted
  | Modified
  | Renamed
  | Copied
  | Ignored
  | Untracked
  | Typechange
  | Unreadable
  | Conflicted
  deriving stock (Show)

data DeltaInfo = DeltaInfo {
  status :: DeltaStatus,
  oldPath :: FilePath,
  newPath :: FilePath
} deriving stock (Show)

data StatusEntryDeltaInfo = StatusEntryDeltaInfo {
  headToIndex :: Maybe DeltaInfo,
  indexToWorkDir :: Maybe DeltaInfo
} deriving stock (Show)

newtype StatusInfo = StatusInfo {
  delta :: [StatusEntryDeltaInfo]
} deriving stock (Show)