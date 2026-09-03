{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Budgeting where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Budget a = Budget
  { totalLimit :: Int
  , categoryLimits :: Map a Int
  } deriving (Show, Eq)

{-While the original problem specification has count and categories optional,
  we may deal with this in a hypothetical parsing step, setting count to 1 if
  missing, and likewise setting categories to the empty set if missing.-}
data Item a = Item
  { itemCost :: Int
  , itemCount :: Int
  , itemCategories :: Set a
  } deriving (Show, Eq)

type Bill a = [Item a]

billCost bill = sum $ fmap (liftA2 (*) itemCost itemCount) bill

categoryCost cat =
  sum .
  fmap (liftA2 (*) itemCost itemCount) .
  filter (Set.member cat . itemCategories)

canAfford budget bill =
  billCost bill <= totalLimit budget &&
  all
    (\(cat, lim) -> categoryCost cat bill <= lim)
    (Map.toList $ categoryLimits budget)
  -- (null (categoryLimits budget) ||
  --  any
  --   (\(cat, lim) -> categoryCost cat bill <= lim)
  --   (Map.toList $ categoryLimits budget))
