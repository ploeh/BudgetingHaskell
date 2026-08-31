{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Budgeting where

import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap, (!))
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

itemTotal = liftA2 (*) itemCost itemCount

billCost = sum . fmap itemTotal

categoryCost cat =
  sum .
  fmap itemTotal .
  filter (Set.member cat . itemCategories)

addItemCategory cat item =
  item { itemCategories = Set.insert cat (itemCategories item) }

canAfford :: Budget -> Bill -> Bool
canAfford budget bill =
  billCost bill <= totalLimit budget &&
  all (fitsWithin maximum . pure) bill &&
  fitsWithin sum (filter charged bill)
  where
    limits = categoryLimits budget
    charged = not . Set.null . budgetedCategories limits
    {-Items are charged to the limits of the categories they are budgeted in; a
      single item goes to one of them, and together they share them all. Without
      any such category no limit applies, and the group always fits.-}
    fitsWithin combine group = null caps || billCost group <= combine caps
      where
        caps =
          fmap (limits !) $ toList $ foldMap (budgetedCategories limits) group

{-An item can only be charged to a category the budget actually limits. Any
  other category of the item is unrestricted, so no limit applies there.-}
budgetedCategories limits =
  Set.filter (`HashMap.member` limits) . itemCategories