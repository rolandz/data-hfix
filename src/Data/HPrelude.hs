module Data.HPrelude
  ( type (->.)
  , type (:*.)(..)
  , Const(..)
  , HFunctor(..)
  , Some(..)
  , unConst
  , HEq(..)
  , HOrd(..)
  , HOrdering(..)
  , unHOrdering
  , (:~:)(..)
  ) where

import Data.Maybe
import Data.Type.Equality

type f ->. g = forall ix. f ix -> g ix
infixr 0 ->.

data (f :*. g) a = f a :&. g a
infixr 5 :&.

instance (Functor f, Functor g) => Functor (f :*. g) where fmap f (x :&. y) = fmap f x :&. fmap f y

data Some f = forall a. Some (f a)

class HFunctor f where hmap :: (a ->. b) -> f a ->. f b

newtype Const a b = Const a deriving (Eq, Ord, Show, Functor)

unConst :: Const a b -> a
unConst (Const x) = x

------------------------------------------------------------------------
-- HEq and HOrd

-- a :=: b is the type of witnesses proving that the types a and b are
-- the same.
--data (:=:) a :: * -> * where Refl :: a :=: a

class HEq a where (==.) :: a ix1 -> a ix2 -> Maybe (ix1 :~: ix2)
class HEq a => HOrd a where hcompare :: a ix1 -> a ix2 -> HOrdering ix1 ix2

infix 4 ==.

data HOrdering a b = HLt | HEq (a :~: b) | HGt

unHOrdering :: HOrdering a b -> Ordering
unHOrdering (HEq _) = EQ
unHOrdering HLt = LT
unHOrdering HGt = GT

instance HEq f => Eq (Some f) where Some x == Some y = isJust $ x ==. y
instance HOrd f => Ord (Some f) where Some x `compare` Some y = unHOrdering $ hcompare x y
