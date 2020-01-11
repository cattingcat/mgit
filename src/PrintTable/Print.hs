module PrintTable.Print (
  Endl,
  type (:|:),
  printTable,

  CellSizingStrategy(..),
  Accessors(..),
  Cell(..)
) where

import Prelude (($), mapM_)
import GHC.Base (IO, error, fmap, (>), (<>))
import Data.Int (Int)
import Control.Category ((.))
import Data.Ord (max, min)
import GHC.List (foldr, zip)
import GHC.Num ((-))
import Data.Text as T hiding (foldr, zip)
import Data.Text.IO

import PrintTable.SizingStrategies
import PrintTable.Cell


data Endl

infixr 5 :|:
data a :|: b

type family Strategies spec :: [CellSizingStrategy] where
   Strategies (s :|: Endl) = '[s]
   Strategies (s :|: ss) = s ': Strategies ss


printTable :: forall spec a . Accessors a (Strategies spec) -> ([a] -> IO ())
printTable accessors as = let
  access :: Accessors a strategy -> a -> [Text]
  access Endl _ = []
  access (C accessor :| cs) a = accessor a : access cs a

  format (str, len) = let
    diff = len - length str
    space n = replicate n " "
    shorten = take len
    formattedStr = if diff > 0 then str <> space diff else shorten str
    in formattedStr <> " "

  formatLine pairs = foldr (<>) "" (fmap format pairs)

  agg = prepareAggregates accessors as
  accessed = fmap (access accessors) as
  withLen = fmap (`zip` agg) accessed
  formatted = fmap formatLine withLen
  in
    mapM_ putStrLn formatted


prepareAggregates :: Accessors a strategies -> [a] -> [Int]
prepareAggregates Endl _ = []
prepareAggregates accessors as = let
  foldFunc :: Accessors a strats -> a -> [Int] -> [Int]
  foldFunc Endl _ _ = []
  foldFunc _ _ [] = error "toEmptyList incorrect"
  foldFunc (accsr@(C accessor) :| cs) a (acc:accs) = let
    sizingStrat = strategyVal accsr
    len = length . accessor $ a
    lens = foldFunc cs a accs
    maxLen = max len acc
    in case sizingStrat of
      StrategyMaxLen  -> maxLen       : lens
      StrategyFixed l -> min maxLen l : lens

  toEmptyList :: Accessors a strats -> [Int]
  toEmptyList Endl = []
  toEmptyList (_ :| cs) = 0 : toEmptyList cs

  in foldr (foldFunc accessors) (toEmptyList accessors) as