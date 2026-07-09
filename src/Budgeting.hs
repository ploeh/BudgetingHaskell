{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Budgeting
  ( CategoryBudget(..)
  , Budget(..)
  , Item(..)
  , addItemCategory
  , Bill
  , billCost
  , canAfford
  ) where

import qualified Data.HashMap.Strict as HM

data CategoryBudget = CategoryBudget
  { category :: String
  , limit :: Int }
  deriving (Show, Eq)

data Budget = Budget
  { totalLimit :: Int
  , categoryLimits :: [CategoryBudget] }
  deriving (Show, Eq)

{-While the original problem specification has count and categories optional,
  we may deal with this in a hypothetical parsing step, setting count to 1 if
  missing, and likewise setting categories to an empty list if missing.-}
data Item = Item 
  { itemCost :: Int
  , itemCount :: Int
  , itemCategories :: [String] }
  deriving (Show, Eq)

addItemCategory :: String -> Item -> Item
addItemCategory cat item = item { itemCategories = cat : itemCategories item }

type Bill = [Item]

billCost bill = sum $ fmap (liftA2 (*) itemCost itemCount) bill

canAfford :: Budget -> Bill -> Bool
canAfford budget bill =
  let total = billCost bill
      -- This is not yet the correct algorithm, but in true TDD fashion, it
      -- passes all tests.
      categoryTotals =
        HM.fromListWith (+) [
          (cat, itemCost item * itemCount item) |
          item <- bill, cat <- itemCategories item
        ]
      categoryLimitsMap =
        HM.fromList [(category cb, limit cb) | cb <- categoryLimits budget]
      finalLimits = HM.intersectionWith (-) categoryLimitsMap categoryTotals
  in all (0 <=) (HM.elems finalLimits) && total <= totalLimit budget