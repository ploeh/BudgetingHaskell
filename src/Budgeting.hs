{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Budgeting where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (partition, sort, sortOn)
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
  split into independent groups, each paired with the categories it spans.-}
components = foldr addItem []
  where
    addItem item groups =
      let cats = itemCategories item
          (touching, disjoint) = partition (not . Set.disjoint cats . fst) groups
          merged =
            ( Set.unions (cats : fmap fst touching)
            , item : concatMap snd touching )
      in merged : disjoint

chargeable limits (cats, items) =
  fst $ charge Set.empty caps $ sortOn (negate . itemTotal) items
  where
    caps =
      HashMap.fromList
        [(c, HashMap.lookupDefault 0 c limits) | c <- Set.toList cats]

{-Charges each item to one of its categories, backtracking when a choice turns
  out not to work. Already refuted states are remembered, so that they aren't
  explored again.-}
charge failed _ [] = (True, failed)
charge failed caps items@(item:rest)
  | sum (itemTotal <$> items) > sum (HashMap.elems caps) = (False, failed)
  | not (Set.null failed) && Set.member key failed = (False, failed)
  | otherwise =
      case safeCharge [] items of
        Just (cat, i, others) -> charge failed (spend cat (itemTotal i)) others
        Nothing -> tryCategories failed candidates
  where
    key =
      ( sort [(itemTotal i, Set.toAscList (itemCategories i)) | i <- items]
      , sortOn fst (HashMap.toList caps) )
    cost = itemTotal item
    remaining cat = HashMap.lookupDefault 0 cat caps
    spend cat amount = HashMap.adjust (subtract amount) cat caps
    loads =
      HashMap.fromListWith (+)
        [(cat, itemTotal i) | i <- items, cat <- Set.toList (itemCategories i)]
    {-A category with room for every remaining item in it is always a safe
      choice: any other charge could be moved there. Such a charge therefore
      needs no backtracking.-}
    safeCharge _ [] = Nothing
    safeCharge seen (i:unseen) =
      case filter isSafe (Set.toList (itemCategories i)) of
        (cat:_) -> Just (cat, i, reverse seen ++ unseen)
        []      -> safeCharge (i:seen) unseen
      where isSafe cat = HashMap.lookupDefault 0 cat loads <= remaining cat
    -- Prefer the tightest fit, leaving as much room as possible for later items.
    candidates =
      sortOn remaining $
      filter ((cost <=) . remaining) $
      Set.toList (itemCategories item)
    tryCategories failed' [] = (False, Set.insert key failed')
    tryCategories failed' (cat:cats') =
      case charge failed' (spend cat cost) rest of
        (True, failed'') -> (True, failed'')
        (False, failed'') -> tryCategories failed'' cats'