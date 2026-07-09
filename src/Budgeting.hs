{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Budgeting
  ( CategoryBudget(..)
  , Budget(..)
  , Item(..)
  , Bill
  , billCost
  , canAfford
  ) where

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

type Bill = [Item]

billCost bill = sum $ fmap (liftA2 (*) itemCost itemCount) bill

canAfford :: Budget -> Bill -> Bool
canAfford _ _ = True