{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Budgeting where

import Data.List (sortOn)
import Data.Map.Strict (Map, (!))
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

itemTotal = liftA2 (*) itemCost itemCount

billCost bill = sum $ fmap itemTotal bill

categoryCost cat =
  sum . fmap itemTotal . filter (Set.member cat . itemCategories)

canAfford budget bill =
  billCost bill <= totalLimit budget &&
  assign explodedItems limits
  where
    limits = categoryLimits budget
    {-An item is only constrained if it has categories that are limited by the
      budget. Any category not shared by the budget are considered irrelevant
      and removed by keeping only the intersection.-}
    forcedItems =
      sortOn (negate . itemTotal) $
      filter (not . Set.null . itemCategories)
      [ i { itemCategories =
              Set.intersection (itemCategories i) (Map.keysSet limits) }
      | i <- bill
      ]
    {-Per one of the ROT13 spoilers, items may be distributed across multiple
      categories, so we explode each item into multiple single-count items.-}
    explodedItems =
      forcedItems >>= (\i -> replicate (itemCount i) i { itemCount = 1 })

    -- Each item must fit, in its entirety, within a single one of its
    -- categories, so try every assignment until one of them works out.
    assign [] _ = True
    assign (item:rest) remaining =
      or
        [ assign rest (Map.adjust (subtract $ itemTotal item) cat remaining)
        | cat <- Set.toList (itemCategories item)
        , remaining ! cat >= itemTotal item
        ]

