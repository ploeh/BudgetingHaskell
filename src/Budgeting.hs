{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Budgeting where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set

data Budget = Budget
  { totalLimit :: Int
  , categoryLimits :: HashMap String Int
  } deriving (Show, Eq)

{-While the original problem specification has count and categories optional,
  we may deal with this in a hypothetical parsing step, setting count to 1 if
  missing, and likewise setting categories to the empty set if missing.-}
data Item = Item
  { itemCost :: Int
  , itemCount :: Int
  , itemCategories :: Set String
  } deriving (Show, Eq)

type Bill = [Item]

billCost = sum . fmap (liftA2 (*) itemCost itemCount)

categoryCost cat =
  sum .
  fmap (liftA2 (*) itemCost itemCount) .
  filter (Set.member cat . itemCategories)

canAfford :: Budget -> Bill -> Bool
canAfford budget bill =
  let canAffordCategory cat limit = categoryCost cat bill <= limit
  in billCost bill <= totalLimit budget &&
      all (uncurry canAffordCategory) (HashMap.toList $ categoryLimits budget)