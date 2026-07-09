import Budgeting
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

newtype UncategorizedItem =
  UncategorizedItem { getUncategorizedItem :: Item } deriving (Show, Eq)

instance Arbitrary UncategorizedItem where
  arbitrary = do
    Positive cost <- arbitrary
    Positive count <- arbitrary
    return $ UncategorizedItem $ Item cost count []

newtype UncategorizedBill =
  UncategorizedBill { getUncategorizedBill :: Bill } deriving (Show, Eq)

instance Arbitrary UncategorizedBill where
  arbitrary = do
    items <- listOf1 (getUncategorizedItem <$> arbitrary)
    return $ UncategorizedBill items

main :: IO ()
main = defaultMain
  [
    testProperty "Affordable uncategorized bill" $ do
      UncategorizedBill bill <- arbitrary
      let cost = billCost bill
      NonNegative surplus <- arbitrary
      categories <- listOf (CategoryBudget <$> arbitrary <*> arbitrary)
      let budget = Budget (cost + surplus) categories

      let actual = canAfford budget bill

      let errMsg = "budget: " ++ show budget ++ ", bill: " ++ show bill
      return $ counterexample errMsg actual
    ,
    testProperty "Unaffordable uncategorized bill" $ do
      UncategorizedBill bill <- arbitrary
      let cost = billCost bill
      Positive deficit <- arbitrary
      categories <- listOf (CategoryBudget <$> arbitrary <*> arbitrary)
      let budget = Budget (cost - deficit) categories

      let actual = canAfford budget bill

      let errMsg = "budget: " ++ show budget ++ ", bill: " ++ show bill
      return $ counterexample errMsg $ not actual
    ,
    testProperty "Bill unaffordable because of category limit" $ do
      UncategorizedBill ubill <- arbitrary
      let cost = billCost ubill
      cat <- listOf1 arbitraryPrintableChar
      Positive deficit <- arbitrary
      let catBudget = CategoryBudget cat (cost - deficit)
      let bill = fmap (addItemCategory cat) ubill
      NonNegative surplus <- arbitrary
      let budget = Budget (cost + surplus) [catBudget]

      let actual = canAfford budget bill

      let errMsg = "budget: " ++ show budget ++ ", bill: " ++ show bill
      return $ counterexample errMsg $ not actual
  ]
