import Data.Monoid (Sum(..))
import Dict

-- Arithmetic contract: result is exactly (dirty - accruals + initial)
{-@ clean :: dirty:Double -> accruals:Double -> initial:Double
          -> {r:Double | r == dirty - accruals + initial} @-}
clean :: Double -> Double -> Double -> Double
clean dirty accruals initial = dirty - accruals + initial

-- SPEC: result is Just iff the key exists in ANY of the three dicts.
-- Dict.map preserves keys, and <> (unionWith) keeps keys from either side,
-- so a key present in dirty, accruals, OR initial will appear in the merged dict.
-- This is the key semantic difference from comp2 (requires all three) and
-- comp3 (requires dirty but not accruals/initial).
{- @ comp1 :: key:String
          -> dirty:Dict String (Sum Double)
          -> accruals:Dict String (Sum Double)
          -> initial:Dict String (Sum Double)
          -> {r:Maybe Double | isJust r <=>
               (Map.member key dirty || Map.member key accruals || Map.member key initial)}
  @-}
comp1 ::
    String
    -> Dict String (Sum Double)
    -> Dict String (Sum Double)
    -> Dict String (Sum Double)
    -> Maybe Double
comp1 key dirty accruals initial =
    let cleans = dirty <> Dict.map negate accruals <> initial
    in fmap getSum (Dict.lookup key cleans)

-- SPEC: result is Just iff ALL three inputs are Just.
-- Missing any one makes the whole computation Nothing.
{-@ comp2 :: dirty:Maybe Double
          -> accruals:Maybe Double
          -> initial:Maybe Double
          -> {r:Maybe Double | isJust r <=> (isJust dirty && isJust accruals && isJust initial)}
  @-}
comp2 :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double
-- Original (LH cannot verify without Applicative specs):
--   comp2 dirty accruals initial = pure clean <*> dirty <*> accruals <*> initial
comp2 dirty accruals initial =
    case (dirty, accruals, initial) of
        (Just d, Just a, Just i) -> Just (clean d a i)
        _                        -> Nothing

-- SPEC: result is Just iff dirty is Just.
-- accruals and initial default to 0, so they never cause Nothing.
{-@ comp3 :: dirty:Maybe Double
          -> accruals:Maybe Double
          -> initial:Maybe Double
          -> {r:Maybe Double | isJust r <=> isJust dirty}
  @-}
comp3 :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double
-- Original (LH cannot verify without Applicative/maybe specs):
--   comp3 dirty accruals initial =
--       let accruals' = maybe 0 id accruals
--           initial'  = maybe 0 id initial
--       in pure clean <*> dirty <*> pure accruals' <*> pure initial'
comp3 dirty accruals initial =
    let a = case accruals of { Just x -> x; Nothing -> 0 }
        i = case initial  of { Just x -> x; Nothing -> 0 }
    in case dirty of
        Nothing -> Nothing
        Just d  -> Just (clean d a i)

main :: IO ()
main = do
    let dirtyDict = Dict.fromList [("account1", Sum 100.0)]
        accrualsDict = Dict.fromList [("account1", Sum 20.0)]
        initialDict = Dict.empty
    print $ "COMP1: " ++ show (comp1 "account1" dirtyDict accrualsDict initialDict)
    print $ "COMP2: " ++ show (comp2 (Just 100.0) (Just 20.0) Nothing)
    print $ "COMP3: " ++ show (comp3 (Just 100.0) (Just 20.0) Nothing)
