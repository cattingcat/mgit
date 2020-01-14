module MGit.MonadMassAction (
  MonadMassAction(..)
) where

import Control.Monad (Monad)


class (Monad m, Monad g) => MonadMassAction m g | g -> m where
  mrun :: m a -> g [a]