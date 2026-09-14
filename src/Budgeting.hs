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
  assign (zip [0 :: Int ..] forcedItems) limits
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

    assign items remaining = fst $ go items remaining Set.empty

    {-Try every distribution of every item until one of them works out. Failed
      (item, remaining limits) states are memoised, since different orders of
      choices often lead to the same state.-}
    go [] _ seen = (True, seen)
    go ((i, item) : rest) remaining seen
      | Set.member key seen = (False, seen)
      | otherwise =
          case tryAll (distributions item remaining) seen of
            (False, seen') -> (False, Set.insert key seen')
            success -> success
      where
        key = (i, remaining)
        tryAll [] s = (False, s)
        tryAll (remaining' : rest') s =
          case go rest remaining' s of
            (False, s') -> tryAll rest' s'
            success -> success

{-Per one of the ROT13 spoilers, the units of an item with a count may be
  distributed across several categories. Since the units are interchangeable,
  only the number of units per category matters, so enumerate those counts
  rather than each unit's individual assignment.-}
distributions (Item cost count categories) =
  go (Set.toList categories) count
  where
    go [] n m = [m | n == 0]
    -- The last category has to absorb whatever units are left.
    go [cat] n m = [Map.insert cat (m ! cat - n * cost) m | m ! cat >= n * cost]
    go (cat : cats) n m =
      [ m'
      | k <- [0 .. maxUnits (m ! cat)]
      , m' <- go cats (n - k) (Map.insert cat (m ! cat - k * cost) m)
      ]
      where
        maxUnits limit
          | cost <= 0 = n
          | otherwise = min n $ max 0 $ limit `div` cost

