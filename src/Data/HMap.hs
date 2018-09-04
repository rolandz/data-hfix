module Data.HMap
  ( HMap
  , empty
  , insert
  , delete
  , lookup
  ) where

import Data.HPrelude
import Data.Map (Map)
import Prelude hiding (lookup)
import qualified Data.Map as Map

-- Implementation on top of Map, a bit clumsy: to save the index
-- relationship between keys and values each key is stored a second
-- time along with the value. Also, lookups need to perform an extra
-- heq comparison in order to establish the index relationship between
-- the given key and the stored key.

newtype HMap k v = HMap (Some k `Map` Entry k v)

data Entry k v = forall ix. Entry (k ix) (v ix)

empty :: HMap k v
empty = HMap Map.empty

insert :: HOrd k => k ix -> v ix -> HMap k v -> HMap k v
insert k v (HMap m) = HMap $ Map.insert (Some k) (Entry k v) m

delete :: HOrd k => k ix -> HMap k v -> HMap k v
delete k (HMap m) = HMap $ Map.delete (Some k) m

lookup :: HOrd k => k ix -> HMap k v -> Maybe (v ix)
lookup k (HMap m) = case Map.lookup (Some k) m of
  Nothing -> Nothing
  Just (Entry k' v) -> case k ==. k' of
    Nothing -> error "impossible" -- This means that `hcompare x y = HEq _` does not imply `(x ==. y) == Just _`
    Just Refl -> Just v
