module PrintTable.SizingStrategies (
  CellSizingStrategy(..),
  Strategy(..),
  KnownSizingStrategy(..)
) where

import GHC.Base (Nat)
import Data.Int (Int)
import GHC.Show (Show)


data CellSizingStrategy = Pad Nat | Fixed Nat | MaxLen

data Strategy = StrategyFixed Int | StrategyMaxLen
  deriving stock (Show)

class KnownSizingStrategy s where
  strategyVal :: s -> Strategy