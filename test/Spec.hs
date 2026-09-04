{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
import Budgeting
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word8)
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

newtype AnyItem a = AnyItem { getItem :: Item a } deriving (Show, Eq)

instance (Arbitrary a, Ord a) => Arbitrary (AnyItem a) where
  arbitrary = do
    Positive cost <- arbitrary
    Positive count <- arbitrary
    AnyItem . Item cost count <$> arbitrary
  shrink (AnyItem (Item cost count categories)) =
    [ AnyItem $ Item cost' count' categories'
    | (Positive cost', Positive count', categories') <-
        shrink (Positive cost, Positive count, categories)
    ]

newtype AnyBudget a = AnyBudget { getBudget :: Budget a } deriving (Show, Eq)

instance (Arbitrary a, Ord a) => Arbitrary (AnyBudget a) where
  arbitrary = do
    Positive tl <- arbitrary
    cls <- fmap getPositive . Map.fromList <$> arbitrary
    return $ AnyBudget $ Budget tl cls
  shrink (AnyBudget (Budget tl cls)) =
    [ AnyBudget $ Budget tl' (fmap getPositive . Map.fromList $ cls')
    | (Positive tl', cls') <-
        shrink (Positive tl, Map.toList (fmap Positive cls))
    ]

main :: IO ()
main = defaultMain
  [ testProperty "Uncategorized bill is only determined by its cost" $ \
      (fmap getItem -> bill :: Bill String) (NonNegative lim) ->
      let budget = Budget lim (mempty :: Map String Int)

          bill' = (\i -> i { itemCategories = mempty }) <$> bill
          actual = canAfford budget bill'

          expected = billCost bill' <= lim
      in counterexample ("Budget: " ++ show budget) $
         counterexample ("Bill: " ++ show bill') $
         expected === actual
    ,
    testProperty "Forced category limit applies" $ \
      (fmap getItem . getNonEmpty -> bill :: Bill Char)
      (NonNegative catLim) ->
      Set.unions (itemCategories <$> bill) /= mempty ==>
      do
        cat <- elements $ Set.toList $ Set.unions (itemCategories <$> bill)
        -- Force the selected category by making it the only category for those
        -- items that have it.
        let bill' = (\i -> if Set.member cat (itemCategories i)
                          then i { itemCategories = Set.singleton cat }
                          else i) <$> bill
            budget = Budget (billCost bill') (Map.singleton cat catLim)

            actual = canAfford budget bill'

            expected = categoryCost cat bill' <= catLim
        return $ counterexample ("Budget: " ++ show budget) $
                 counterexample ("Bill: " ++ show bill') $
                 expected === actual
    ,
    testProperty "Monotonocity: Removing item from affordable bill can never make it unaffordable" $ \
      (AnyBudget (budget :: Budget String))
      (fmap getItem . getNonEmpty -> bill :: Bill String) ->
      do
        itemToRemove <- elements bill
        let bill' = filter (/= itemToRemove) bill

            actual = canAfford budget bill'

        return $ counterexample ("Modified bill: " ++ show bill') $
                 not (canAfford budget bill) || actual
    ,
    testProperty "Monotonocity: Adding category to affordable bill never makes it unaffordable" $ \
      (Positive totLim)
      (fmap getPositive . Map.fromList . getNonEmpty -> cls)
      (fmap getItem . getNonEmpty -> bill :: Bill Word8)
      newCat
      (Positive lim) ->
      withMaxSuccess 10_000 $
      -- Filter out the case where the new category is already present in the
      -- budget, since that would not be a valid test of adding a NEW limit:
      not (Map.member newCat cls) ==>
      let -- Only keep those items that are already covered by the existing
          -- budget categories. If these are already affordable, then adding a
          -- new category limit should not make them unaffordable:
          existingCats = Set.fromList $ Map.keys cls
          isAlreadyCovered =
            not . Set.null . Set.intersection existingCats . itemCategories
          bill' = filter isAlreadyCovered bill
          -- Now add the new category limit to the budget:
          budget = Budget totLim (Map.insert newCat lim cls)

          actual = canAfford budget bill'

      in counterexample ("Modified budget: " ++ show budget) $
         counterexample ("Modified bill: " ++ show bill') $
         not (canAfford (Budget totLim cls) bill') || actual
  ]