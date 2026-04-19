module Dict where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | A Dict that merges values at duplicate keys using their Semigroup instance.
-- This differs from the default Map union which is left-biased (drops duplicates).
newtype Dict k v = Dict { unDict :: Map k v }
    deriving (Show, Eq)

instance (Ord k, Semigroup v) => Semigroup (Dict k v) where
    Dict m1 <> Dict m2 = Dict (Map.unionWith (<>) m1 m2)

-- | Monoid requires an identity element: the empty Dict
instance (Ord k, Semigroup v) => Monoid (Dict k v) where
    mempty = Dict Map.empty

fromList :: Ord k => [(k, v)] -> Dict k v
fromList = Dict . Map.fromList

toList :: Dict k v -> [(k, v)]
toList = Map.toList . unDict

lookup :: Ord k => k -> Dict k v -> Maybe v
lookup k (Dict m) = Map.lookup k m

insert :: (Ord k, Semigroup v) => k -> v -> Dict k v -> Dict k v
insert k v (Dict m) = Dict (Map.insertWith (<>) k v m)

empty :: Dict k v
empty = Dict Map.empty

-- | Transform values, leaving keys untouched
map :: (a -> b) -> Dict k a -> Dict k b
map f (Dict m) = Dict (Map.map f m)

-- | Transform both keys and values; if two keys collide after mapping,
-- their values are combined via Semigroup
mapKeys :: (Ord k2, Semigroup v) => (k1 -> k2) -> Dict k1 v -> Dict k2 v
mapKeys f (Dict m) = Dict (Map.mapKeysWith (<>) f m)

-- | Filter entries by a predicate on the value
filter :: (v -> Bool) -> Dict k v -> Dict k v
filter p (Dict m) = Dict (Map.filter p m)

-- | Fold over all values
foldr :: (v -> b -> b) -> b -> Dict k v -> b
foldr f z (Dict m) = Map.foldr f z m

size :: Dict k v -> Int
size (Dict m) = Map.size m

member :: Ord k => k -> Dict k v -> Bool
member k (Dict m) = Map.member k m

delete :: Ord k => k -> Dict k v -> Dict k v
delete k (Dict m) = Dict (Map.delete k m)