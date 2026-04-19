module Dict where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- Using `data` (not `newtype`) so LH can pattern-match on the constructor for measures.
data Dict k v = Dict { unDict :: Map k v }
    deriving (Show, Eq)

-- LH measure: Maybe in logic (Maybe is a built-in data type, safe to measure)
{-@ measure isJust @-}
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

{-@ assume Map.lookup :: Ord k => k:k -> m:Map k v -> Maybe v @-}
{-@ assume Map.member :: Ord k => k:k -> m:Map k v -> Bool @-}
{-@ assume Map.unionWith :: Ord k
          => (v -> v -> v) -> m1:Map k v -> m2:Map k v -> Map k v @-}

instance (Ord k, Semigroup v) => Semigroup (Dict k v) where
    Dict m1 <> Dict m2 = Dict (Map.unionWith (<>) m1 m2)

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

size :: Dict k v -> Int
size (Dict m) = Map.size m

map :: (a -> b) -> Dict k a -> Dict k b
map f (Dict m) = Dict (Map.map f m)

mapKeys :: (Ord k2, Semigroup v) => (k1 -> k2) -> Dict k1 v -> Dict k2 v
mapKeys f (Dict m) = Dict (Map.mapKeysWith (<>) f m)

-- | Filter entries by a predicate on the value
filter :: (v -> Bool) -> Dict k v -> Dict k v
filter p (Dict m) = Dict (Map.filter p m)

-- | Fold over all values
foldr :: (v -> b -> b) -> b -> Dict k v -> b
foldr f z (Dict m) = Map.foldr f z m

member :: Ord k => k -> Dict k v -> Bool
member k (Dict m) = Map.member k m

delete :: Ord k => k -> Dict k v -> Dict k v
delete k (Dict m) = Dict (Map.delete k m)
