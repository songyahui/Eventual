import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum(..))

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
    -> Map String (Sum Double)
    -> Map String (Sum Double)
    -> Map String (Sum Double)
    -> Maybe Double
comp3 key dirty accruals initial =
    let cleans = dirty <> Map.map negate accruals <> initial
    in fmap getSum (Map.lookup key cleans)