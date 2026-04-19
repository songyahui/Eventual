import Data.Monoid (Sum(..))
import Dict

clean :: Double -> Double -> Double -> Double
clean dirty accruals initial = dirty - accruals + initial

comp1 :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double
comp1 dirty accruals initial =
    pure clean 
        <*> dirty <*> accruals <*> initial

comp2 :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double
comp2 dirty accruals initial =
    let accruals' = maybe 0 id accruals
        initial' = maybe 0 id initial in
    pure clean 
        <*> dirty <*> pure accruals' <*> pure initial'

comp3 ::
    String
    -> Dict String (Sum Double)
    -> Dict String (Sum Double)
    -> Dict String (Sum Double)
    -> Maybe Double
comp3 key dirty accruals initial =
    let cleans = dirty <> Dict.map negate accruals <> initial
    in fmap getSum (Dict.lookup key cleans)

main :: IO ()
main = do
    let dirty = Just 100.0
        accruals = Just 20.0
        initial = Nothing
    print $ comp1 dirty accruals initial
    print $ comp2 dirty accruals initial
    let dirtyDict = Dict.fromList [("account1", Sum 100.0)]
        accrualsDict = Dict.fromList [("account1", Sum 20.0)]
        initialDict = Dict.empty
    print $ comp3 "account1" dirtyDict accrualsDict initialDict