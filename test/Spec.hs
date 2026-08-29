{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
import Budgeting
import Data.Coerce (coerce)
import Data.HashMap.Strict ((!))
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

genPartition :: [a] -> Gen [[a]]
genPartition []     = pure []
genPartition (x:xs) = do
  p <- genPartition xs
  k <- chooseInt (0, length p)
  return $
    case splitAt k p of
      (before, block:after) -> before ++ ((x:block):after)
      (before, []) -> [x] : before

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
    testProperty "Conjunction of disjoint categories" $ \
      (Set.toList . Set.fromList . getNonEmpty -> cats)
      (Blind (InfiniteList (fmap getUncategorizedItem -> ub) _))
      (Blind (InfiniteList (fmap getNonNegative -> catLims) _)) -> do
        p <- fmap Set.fromList <$> genPartition cats
        let bl = zipWith (\catSet item -> item { itemCategories = catSet }) p ub
        let catLimits = HashMap.fromList $ zip cats catLims
        let budget = Budget (billCost bl) catLimits

        let actual = canAfford budget bl
        
        let excesses =
              foldr (HashMap.insertWith (-) <*> (`categoryCost` bl)) catLimits cats
        let expected =
              all (any (\cat -> excesses ! cat <= 0) . itemCategories) bl
        return $
          counterexample ("Bill: " ++ show bl) $
          counterexample ("Budget: " ++ show budget) $
          expected === actual
    ,
    testProperty "Affordability preserved under wider choice" $ \
      (CategorizedBill bill) (NonNegative slack) -> do
        let cats = Set.unions $ itemCategories <$> bill
        let catLimits =
              (+ slack) <$>
              foldr (HashMap.insertWith (+) <*> (`categoryCost` bill)) mempty cats
        -- Affordable by construction:
        let budget = Budget (billCost bill + slack) catLimits

        cat <- elements $ Set.toList cats
        let bill' = fmap (addItemCategory cat) bill
        let actual = canAfford budget bill'

        return $
          counterexample ("Bill: " ++ show bill') $
          counterexample ("Budget: " ++ show budget)
          actual
  ]
