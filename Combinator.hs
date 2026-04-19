import Data.Monoid (Sum(..))
import Dict

clean :: Double -> Double -> Double -> Double
clean dirty accruals initial = dirty - accruals + initial

-- old code
comp1 ::
    String
    -> Dict String (Sum Double)
    -> Dict String (Sum Double)
    -> Dict String (Sum Double)
    -> Maybe Double
comp1 key dirty accruals initial =
    let cleans = dirty <> Dict.map negate accruals <> initial
    in fmap getSum (Dict.lookup key cleans)

-- simplified new code, but it doesn't handle missing keys as well as comp1
comp2 :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double
comp2 dirty accruals initial =
    pure clean 
        <*> dirty <*> accruals <*> initial

-- patched code that handles missing keys by treating them as zero
comp3 :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double
comp3 dirty accruals initial =
    let accruals' = maybe 0 id accruals
        initial' = maybe 0 id initial in
    pure clean 
        <*> dirty <*> pure accruals' <*> pure initial'

main :: IO ()
main = do
    let dirtyDict = Dict.fromList [("account1", Sum 100.0)]
        accrualsDict = Dict.fromList [("account1", Sum 20.0)]
        initialDict = Dict.empty
    print $ "COMP1: " ++ show (comp1 "account1" dirtyDict accrualsDict initialDict)
    print $ "COMP2: " ++ show (comp2 (Just 100.0) (Just 20.0) Nothing)
    print $ "COMP3: " ++ show (comp3 (Just 100.0) (Just 20.0) Nothing)
