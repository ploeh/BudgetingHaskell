{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Budgeting
import Data.Foldable (foldrM)
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
      (fmap getItem . getNonEmpty -> bill :: Bill (Maybe Word8))
      (Positive lim) ->
      let -- Only keep those items that are already covered by the existing
          -- budget categories, except Nothing, which is going to be the new
          -- category.
          -- If these items are already affordable, then adding a  new category
          -- limit should not make them unaffordable.
          -- This step most importantly removes any items that are ONLY in the
          -- new category, because those items could become unaffordable if the
          -- new category limit is too low.
          existingCats =
            Set.delete Nothing $ Set.unions $ fmap itemCategories bill
          isAlreadyCovered =
            not . Set.null . Set.intersection existingCats . itemCategories
          bill' = filter isAlreadyCovered bill
          -- Now construct the first budget so that the bill is affordable:
          categoryCosts =
            Map.delete Nothing $
            foldr (Map.insert <*> (`categoryCost` bill')) mempty $
            Set.unions $
            fmap itemCategories bill'
          budget1 = Budget (billCost bill') categoryCosts

          -- Now add the new category limit to the budget:
          budget2 =
            budget1 {
              categoryLimits = Map.insert Nothing lim (categoryLimits budget1)
            }
          actual = canAfford budget2 bill'

      in canAfford budget1 bill' ==>
         counterexample ("Modified budget: " ++ show budget2) $
         counterexample ("Modified bill: " ++ show bill')
         actual
    ,
    testProperty "Valid assignment is affordable" $ \
      (fmap getItem -> bill :: Bill Word8) ->
      do
        budget <- foldrM genAssignment (Budget 0 mempty) bill
        let actual = canAfford budget bill
        pure $ counterexample ("Generated budget: " ++ show budget) actual
    ,
    testProperty "Budget with zero category limits is never affordable" $ \
      (fmap getItem . getNonEmpty -> bill :: Bill Word8) ->
      let cats = Set.unions $ fmap itemCategories bill
          budget = Budget (billCost bill) $ Map.fromSet (const 0) cats

          actual = canAfford budget bill

      in not (null cats) ==>
         not actual
    ,
    testProperty "An item that fits in no single category is unaffordable" $ \
      (fmap getItem . getNonEmpty -> bill :: Bill Word8) ->
      do
        item <- elements bill
        let cost = itemCost item * itemCount item
            catLimits =
              foldr (\cat acc -> Map.insert cat (cost - 1) acc) mempty $
              itemCategories item

            budget = Budget (billCost bill) catLimits
            actual = canAfford budget bill

        pure $
          not (null $ itemCategories item) ==>
          counterexample ("Budget: " ++ show budget) $ not actual
    ,
    testProperty "Adding unbudgeted category never makes unaffordable bill affordable" $ \
      (fmap getItem . getNonEmpty -> bill :: Bill String)
      newCat ->
        let existingCats = Set.unions $ itemCategories <$> bill
            budget =
              Budget (billCost bill) $
              foldr (`Map.insert` 0) mempty existingCats
            bill' =
              [ i { itemCategories = Set.insert newCat $ itemCategories i }
              | i <- bill
              ]

            actual = canAfford budget bill'
        in
          not (Set.member newCat existingCats) ==>
          (not . all (null . itemCategories)) bill ==>
          counterexample ("Budget: " ++ show budget) $
          counterexample ("Bill: " ++ show bill') $
          not actual
  ]

genAssignment :: Ord a => Item a -> Budget a -> Gen (Budget a)
genAssignment (Item cost count cats) budget =
  if null cats
  then
    pure $ budget { totalLimit = totalLimit budget + cost * count }
  else do
    cat <- elements (Set.toList cats)
    NonNegative slack <- arbitrary
    pure $ budget {
      totalLimit = totalLimit budget + cost * count,
      categoryLimits =
        Map.insertWith (+) cat (cost * count + slack) (categoryLimits budget) }