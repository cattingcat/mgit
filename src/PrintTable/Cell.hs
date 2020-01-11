module PrintTable.Cell (
  Cell(..),
  KnownSizingStrategy(..),
  Accessors(..)
) where

import Prelude ()

import PrintTable.SizingStrategies
import GHC.TypeNats (KnownNat, natVal)
import Data.Data (Proxy(..))
import GHC.Real (fromIntegral)
import Control.Category ((.))
import Data.Function (($))
import Data.Text


newtype Cell a (strategy :: CellSizingStrategy) = C (a -> Text)

instance KnownSizingStrategy (Cell a 'MaxLen) where
  strategyVal _ = StrategyMaxLen

instance KnownNat len => KnownSizingStrategy (Cell a ('Fixed len)) where
  strategyVal _ = StrategyFixed . fromIntegral . natVal $ Proxy @len

infixr 5 :|
data Accessors a (strategies :: [CellSizingStrategy]) where
  (:|) :: KnownSizingStrategy (Cell a s) => Cell a s -> Accessors a ss -> Accessors a (s ': ss)
  Endl :: Accessors a '[]