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

data Node a = Source | ItemNode Int | CatNode a | Sink deriving (Show, Eq, Ord)

canAfford budget bill =
  billCost bill <= totalLimit budget &&
  maxFlow network Source Sink == billCost forcedItems
  where
    limits = categoryLimits budget
    forcedItems =
      filter
        (liftA2 (&&) (not . Set.null) (`Set.isSubsetOf` Map.keysSet limits) .
         itemCategories)
        bill
    network = foldr addEdge Map.empty $
      [ (Source, ItemNode i, itemCost item * itemCount item)
      | (i, item) <- zip [0 ..] forcedItems
      ] ++
      [ (ItemNode i, CatNode cat, itemCost item * itemCount item)
      | (i, item) <- zip [0 ..] forcedItems
      , cat <- Set.toList (itemCategories item)
      ] ++
      [ (CatNode cat, Sink, lim) | (cat, lim) <- Map.toList limits ]

addEdge (from, to, cap) =
  Map.insertWith (Map.unionWith (+)) from (Map.singleton to cap) .
  Map.insertWith (Map.unionWith (+)) to (Map.singleton from 0)

-- Ford-Fulkerson with breadth-first (shortest) augmenting paths.
maxFlow graph source sink = go graph 0
  where
    go residual flow = case augmentingPath residual of
      Nothing -> flow
      Just path ->
        let bottleneck = minimum [residual Map.! from Map.! to | (from, to) <- path]
        in go (foldr (augment bottleneck) residual path) (flow + bottleneck)

    augment amount (from, to) =
      Map.adjust (Map.adjust (subtract amount) to) from .
      Map.adjust (Map.adjust (+ amount) from) to

    augmentingPath residual = search [source] (Map.singleton source source)
      where
        search [] _ = Nothing
        search (node:queue) parents
          | node == sink = Just (pathTo node)
          | otherwise = search (queue ++ next) parents'
          where
            next =
              [ to
              | (to, cap) <- Map.toList $ Map.findWithDefault mempty node residual
              , cap > 0
              , not (Map.member to parents)
              ]
            parents' = foldr (`Map.insert` node) parents next
            pathTo to
              | to == source = []
              | otherwise = let from = parents Map.! to in (from, to) : pathTo from
