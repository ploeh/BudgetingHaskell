{-# LANGUAGE TypeApplications #-}
import Budgeting
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

newtype UncategorizedItem =
  UncategorizedItem { getUncategorizedItem :: Item } deriving (Show, Eq)

instance Arbitrary UncategorizedItem where
  arbitrary = do
    Positive cost <- arbitrary
    Positive count <- arbitrary
    return $ UncategorizedItem $ Item cost count mempty
  shrink (UncategorizedItem (Item cost count _)) =
    [ UncategorizedItem $ Item cost' count' mempty
    | (cost', count') <-
        coerce (shrink @(Positive Int, Positive Int)) (cost, count)
    ]

newtype UncategorizedBill =
  UncategorizedBill { getUncategorizedBill :: Bill } deriving (Show, Eq)

instance Arbitrary UncategorizedBill where
  arbitrary = do
    items <- listOf (getUncategorizedItem <$> arbitrary)
    return $ UncategorizedBill items
  shrink (UncategorizedBill items) =
    coerce $ shrinkList (coerce (shrink @UncategorizedItem)) items

newtype CategorizedItem =
  CategorizedItem { getCategorizedItem :: Item } deriving (Show, Eq)

instance Arbitrary CategorizedItem where
  arbitrary = do
    Positive cost <- arbitrary
    Positive count <- arbitrary
    categories <- listOf1 (arbitrary @String)
    return $ CategorizedItem $ Item cost count (Set.fromList categories)
  shrink (CategorizedItem (Item cost count categories)) =
    [ CategorizedItem $ Item cost' count' categories'
    | (cost', count', categories') <-
        coerce (shrink @(Positive Int, Positive Int, Set String))
          (cost, count, categories)
    , not (Set.null categories')
    ]

newtype CategorizedBill =
  CategorizedBill { getCategorizedBill :: Bill } deriving (Show, Eq)

instance Arbitrary CategorizedBill where
  arbitrary = do
    items <- listOf1 (getCategorizedItem <$> arbitrary)
    return $ CategorizedBill items
  shrink (CategorizedBill items) =
    coerce $
    filter (not . null) $
    shrinkList (coerce (shrink @CategorizedItem)) items

main :: IO ()
main = defaultMain
  [ testProperty "Uncategorized bill is only determined by its cost" $
      \(UncategorizedBill bill) (NonNegative l) ->
      let budget = Budget l mempty

          actual = canAfford budget bill

          expected = billCost bill <= l
      in expected === actual
    ,
    testProperty "Categorized bill is also determined by a category limit" $
      \(CategorizedBill bill) (NonNegative cb) -> do
      cat <- elements $ Set.toList $ Set.unions $ itemCategories <$> bill
      let budget = Budget (billCost bill) (HashMap.singleton cat cb)

      let actual = canAfford budget bill

      let expected = categoryCost cat bill <= cb
      return $
        counterexample ("Category: \"" ++ cat ++ "\"") $
        expected === actual
    ,
    testProperty "Affordable bill never unaffordable by additional category" $
      \(CategorizedBill bill) newCat (NonNegative newCatLimit) -> do
      cat <- elements $ Set.toList $ Set.unions $ itemCategories <$> bill
      return $
        Set.notMember newCat (Set.unions $ itemCategories <$> bill) ==>
        let affordableCatLimit = categoryCost cat bill
            catLimits =
              HashMap.fromList [(cat, affordableCatLimit), (newCat, newCatLimit)]
            budget = Budget (billCost bill) catLimits

            actual = canAfford budget (fmap (addItemCategory newCat) bill)

        in counterexample ("Category: \"" ++ cat ++ "\"") actual
  ]
