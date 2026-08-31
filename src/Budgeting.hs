{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Budgeting where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (partition)
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

itemTotal = liftA2 (*) itemCost itemCount

billCost = sum . fmap itemTotal

categoryCost cat =
  sum .
  fmap itemTotal .
  filter (Set.member cat . itemCategories)

addItemCategory cat item =
  item { itemCategories = Set.insert cat (itemCategories item) }

{-A bill is affordable when
  * its total cost stays within the total limit,
  * every item that has budgeted categories fits within one of them, and
  * items that compete for the same categories fit within them together.-}
canAfford :: Budget -> Bill -> Bool
canAfford budget bill =
  billCost bill <= totalLimit budget &&
  all fits constrained &&
  all withinCapacity (components limits constrained)
  where
    limits = categoryLimits budget
    constrained = filter (constrains limits) bill
    fits item =
      any (\cat -> itemTotal item <= HashMap.lookupDefault 0 cat limits)
          (Set.toList (budgetedCategories limits item))
    withinCapacity group =
      billCost group <=
      sum [ HashMap.lookupDefault 0 cat limits
          | cat <- Set.toList (foldMap (budgetedCategories limits) group) ]

{-An item can only be charged to a category the budget actually limits. Any
  other category of the item is unrestricted, so no limit applies there.-}
budgetedCategories limits =
  Set.filter (`HashMap.member` limits) . itemCategories

{-An item only constrains a budget when it shares a category with it. An item
  that belongs only to unbudgeted categories (or to no category at all) can
  always be charged there, since no limit applies.-}
constrains limits = not . Set.null . budgetedCategories limits

{-Items that share no categories can't compete for the same limits, so the bill
  is split into independent groups that can be considered one at a time.-}
components limits = foldr addItem []
  where
    addItem item groups = (item : concat touching) : disjoint
      where (touching, disjoint) = partition (any (shares item)) groups
    shares i j =
      not $
      Set.disjoint (budgetedCategories limits i) (budgetedCategories limits j)