module Dict where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | A Dict that merges values at duplicate keys using their Semigroup instance.
-- This differs from the default Map union which is left-biased (drops duplicates).
newtype Dict k v = Dict { unDict :: Map k v }
    deriving (Show, Eq)

-- LH measure: expose the inner Map size as a refineable quantity
{-@ measure dsize :: Dict k v -> Int
    dsize (Dict m) = mlen m
  @-}

-- LH measure: Map.size lifted to a measure LH understands
{-@ measure mlen :: Map k v -> Int @-}

-- Trust Map primitives (containers has no LH specs bundled)
{-@ assume Map.empty  :: {m:Map k v | mlen m == 0} @-}
{-@ assume Map.size   :: m:Map k v -> {n:Int | n == mlen m && n >= 0} @-}
{-@ assume Map.lookup :: Ord k => k:k -> m:Map k v -> Maybe v @-}
{-@ assume Map.member :: Ord k
                      => k:k -> m:Map k v
                      -> {b:Bool | b <=> isJust (Map.lookup k m)} @-}

-- Axiom: unionWith has a key iff at least one input has it
{-@ assume Map.unionWith :: Ord k
          => (v -> v -> v) -> m1:Map k v -> m2:Map k v
          -> {r:Map k v | forall k. Map.member k r <=> (Map.member k m1 || Map.member k m2)} @-}

-- Axiom: Map.map preserves exactly the same set of keys
{-@ assume Map.map :: (a -> b) -> m:Map k a
          -> {r:Map k b | forall k. Map.member k r <=> Map.member k m} @-}

instance (Ord k, Semigroup v) => Semigroup (Dict k v) where
    Dict m1 <> Dict m2 = Dict (Map.unionWith (<>) m1 m2)

-- | Monoid requires an identity element: the empty Dict
instance (Ord k, Semigroup v) => Monoid (Dict k v) where
    mempty = Dict Map.empty

fromList :: Ord k => [(k, v)] -> Dict k v
fromList = Dict . Map.fromList

toList :: Dict k v -> [(k, v)]
toList = Map.toList . unDict

-- | lookup: returns Nothing iff the key is absent
{-@ lookup :: Ord k => k:k -> d:Dict k v
           -> {r:Maybe v | isJust r <=> member k d} @-}
lookup :: Ord k => k -> Dict k v -> Maybe v
lookup k (Dict m) = Map.lookup k m

insert :: (Ord k, Semigroup v) => k -> v -> Dict k v -> Dict k v
insert k v (Dict m) = Dict (Map.insertWith (<>) k v m)

-- | empty dict has size zero
{-@ empty :: {d:Dict k v | dsize d == 0} @-}
empty :: Dict k v
empty = Dict Map.empty

-- | size is always non-negative
{-@ size :: d:Dict k v -> {n:Int | n == dsize d && n >= 0} @-}
size :: Dict k v -> Int
size (Dict m) = Map.size m

-- | member is true exactly when lookup succeeds
{-@ member :: Ord k => k:k -> d:Dict k v
           -> {b:Bool | b <=> isJust (Dict.lookup k d)} @-}

-- | map preserves membership: key in result iff key in input
{-@ map :: (a -> b) -> d:Dict k a
        -> {r:Dict k b | forall k. member k r <=> member k d} @-}
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

member :: Ord k => k -> Dict k v -> Bool
member k (Dict m) = Map.member k m

delete :: Ord k => k -> Dict k v -> Dict k v
delete k (Dict m) = Dict (Map.delete k m)