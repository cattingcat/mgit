module MGit.MonadMultiRepo where

import System.IO (FilePath)
import Control.Monad (Monad)

class Monad m => MonadMultiRepo m where
  repos :: m [FilePath] 