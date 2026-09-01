{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Budgeting
import Data.Map.Strict (Map)
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
  ]
