{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Budgeting where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (partition, sortOn)
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
  * at least one budgeted category stays within its limit, so that spending
    outside the budgeted categories is still defensible, and
  * every item that has only budgeted categories can be charged to one of them
    without exceeding any category limit.
  Since each such item must be charged in full to a single category, the last
  condition is a packing problem, here solved by backtracking.-}
canAfford :: Budget -> Bill -> Bool
canAfford budget bill =
  billCost bill <= totalLimit budget &&
  (HashMap.null limits || any withinLimit (HashMap.toList limits)) &&
  all (chargeable limits) (components (filter (constrains limits) bill))
  where
    limits = categoryLimits budget
    withinLimit (cat, limit) = categoryCost cat bill <= limit

{-An item only constrains a budget when all of its categories are budgeted. An
  item that belongs to an unbudgeted category (or to no category at all) can
  always be charged there, since no limit applies.-}
constrains limits item =
  not (Set.null cats) && all (`HashMap.member` limits) (Set.toList cats)
  where cats = itemCategories item

{-Items that share no categories can't influence each other, so the bill is
  split into independent groups that can be considered one at a time.-}
components = foldr addItem []
  where
    addItem item groups = (item : concat touching) : disjoint
      where (touching, disjoint) = partition (any (shares item)) groups
    shares i j = not $ Set.disjoint (itemCategories i) (itemCategories j)

{-Charges every item in full to one of its categories, trying the alternatives
  in turn and backtracking when a choice turns out not to work. The most
  expensive items are charged first, since they're the hardest to place.-}
chargeable limits = charge limits . sortOn (negate . itemTotal)
  where
    charge _ [] = True
    charge caps (item:items) =
      or [ charge (HashMap.adjust (subtract cost) cat caps) items
         | cat <- Set.toList (itemCategories item)
         , cost <= HashMap.lookupDefault 0 cat caps ]
      where cost = itemTotal item