{-# LANGUAGE TypeApplications #-}
import Budgeting
import Data.Coerce (coerce)
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

main :: IO ()
main = defaultMain
  [ testProperty "Uncategorized bill is only determined by its cost" $
      \(UncategorizedBill bill) (NonNegative l) ->
      let budget = Budget l mempty

          actual = canAfford budget bill

          expected = billCost bill <= l
      in expected === actual
  ]
