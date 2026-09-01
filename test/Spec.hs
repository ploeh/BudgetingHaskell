{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Budgeting
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
  ]
