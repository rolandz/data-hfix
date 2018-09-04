module Data.HAssocList
  ( HAssocList
  , empty
  , insert
  , delete
  , lookup
  ) where

import Data.HPrelude
import Prelude hiding (lookup)

newtype HAssocList k v = HAssocList [Some (k :*. v)]

empty :: HAssocList k v
empty = HAssocList []

-- Does not overwrite, only shadows.
insert :: k a -> v a -> HAssocList k v -> HAssocList k v
insert k v (HAssocList m) = HAssocList $ Some (k :&. v) : m

-- Deletes the first occurrence, if any.
delete :: HEq k => k ix1 -> HAssocList k v -> HAssocList k v
delete k = \(HAssocList m) -> HAssocList $ go m where
  go [] = []
  go (Some (k' :&. _) : m) | Just _ <- k ==. k' = m
                           | otherwise = go m

-- Retrieves the first occurrence, if any.
lookup :: HEq k => k ix -> HAssocList k v -> Maybe (v ix)
lookup k = \(HAssocList m) -> go m where
  go [] = Nothing
  go (Some (k' :&. v) : m) | Just Refl <- k ==. k' = Just v
                           | otherwise = go m
