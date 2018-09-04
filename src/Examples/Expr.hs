module Examples.Expr where

import Data.HFix

import Control.Monad.Identity

data ExprF :: (* -> *) -> * -> * where
  CnstF  :: a                    -> ExprF e a
  AddF   :: e Int  -> e Int      -> ExprF e Int
  EqF    :: e Int  -> e Int      -> ExprF e Bool
  CondF  :: e Bool -> e a -> e a -> ExprF e a

instance HFunctor ExprF where
  hmap _ (CnstF x    ) = CnstF x
  hmap f (AddF x y   ) = AddF (f x) (f y)
  hmap f (EqF x y    ) = EqF (f x) (f y)
  hmap f (CondF x y z) = CondF (f x) (f y) (f z)

type Expr = HFix ExprF

cnst :: a -> Expr a
cnst = HFix . CnstF

add :: Expr Int -> Expr Int -> Expr Int
add = (HFix .) . AddF

eq :: Expr Int -> Expr Int -> Expr Bool
eq = (HFix .) . EqF

cond :: Expr Bool -> Expr a -> Expr a -> Expr a
cond = ((HFix .) .) . CondF

eval :: Expr t -> t
eval = runIdentity . hcata evalF where
  evalF :: ExprF Identity ->. Identity
  evalF (CnstF x    ) = return x
  evalF (AddF  x y  ) = (+) <$> x <*> y
  evalF (EqF   x y  ) = (==) <$> x <*> y
  evalF (CondF x y z) = (\c a b -> if c then a else b) <$> x <*> y <*> z

size :: Expr t -> Int
size = unConst . hcata sizeF where
  sizeF :: ExprF (Const Int) ->. Const Int
  sizeF (CnstF _    ) = Const 1
  sizeF (AddF  x y  ) = Const 1 +. x +. y
  sizeF (EqF   x y  ) = Const 1 +. x +. y
  sizeF (CondF x y z) = Const 1 +. x +. y +. z

  Const a +. Const b = Const $ a + b

e :: Expr Int
e = cond ((cnst 1 `add` cnst 1) `eq` cnst 2) (cnst 3) (cnst 4)
