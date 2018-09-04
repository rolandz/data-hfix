module Data.HFix
  ( HFix(..)
  , hcata
  , hana
  , hhylo
  , module Data.HPrelude
  ) where

import Data.HPrelude

data HFix (f :: (* -> *) -> * -> *) ix = HFix { unHFix :: f (HFix f) ix }

hcata :: HFunctor f => (f a ->. a) -> (HFix f ->. a)
hcata f = f . hmap (hcata f) . unHFix

hana :: HFunctor f => (a ->. f a) -> (a ->. HFix f)
hana f = HFix . hmap (hana f) . f

hhylo :: HFunctor f => (f b ->. b) -> (a ->. f a) -> (a ->. b)
hhylo phi psi = hcata phi . hana psi
