module Expr
  ( DeBruijn (..)
  , Expr (..)
  , BinOp (..)
  , Pred (..)
  , Subable (..)
  ) where

-- | Allows for variable substitutions.
class Subable s where
  -- | Shift variables.
  --
  -- The shift function is a helper function used to implement substitution.
  -- Its purpose is to avoid variable capture in the substituent (i.e. the new expression 
  -- we are inserting in place of the variable). Shift should thus only be
  -- called when we encounter a term that binds variables, like forall.
  --
  -- The shift traverses the current structure with a variable (x :@ i) we
  -- intend to shift. If we encounter a variable (y :@ j), we will increment
  -- the index j if the variable names x and y are equivalent and j is larger
  -- or equal to i.
  --
  -- That is, we shift all the free occurences of variable x. If j was smaller
  -- than i, (y :@ j) is a bound variable, thus we do not increment.
  --
  -- If we encounter a variable declaration, we will continue the recursion
  -- using index i + 1.
  shift :: Eq a => DeBruijn a -> s a -> s a

  -- | Variable substitution.
  --
  -- Substitutes an expression for the given variable. To understand how
  -- variable substitution works w.r.t. De Bruijn indices, look at the
  -- explanation of the De Bruijn data structure.
  --
  -- Given our variable to substitute (x :@ i) and substituent, the
  -- implementation would recursively traverse the datastructure. If a variable
  -- is encountered that is equivalent to our given variable (x :@ i), we
  -- substitute.
  --
  -- For any variable declaration, we would need to shift our substituent and if
  -- required (i.e. the declaration is equivalent to x) also increment index i.
  subst' :: Eq a => DeBruijn a -> Expr a -> s a -> s a

  -- | Top level substitution
  --
  -- Substitutions at the top level are almost exclusively with a De Bruijn
  -- index of 0. Hence, we provide a shorthand.
  subst :: Eq a => a -> Expr a -> s a -> s a
  subst x = subst' (x :@ 0)

-- | De Bruijn indexed variable.
--
-- De Bruijn indices are one of many ways to avoid incorrect variable capture
-- in substitions.
--
-- The index tracks which binder we refer to in the tree. To illustrate,
-- given the following two expressions:
--
-- e1 = forall x. x :@ 0
-- e2 = forall x. x :@ 1
--
-- Expression e1 has no free variable occurences, x :@ 0 refers to the variable
-- bound by the forall quantifier.
--
-- Expression e2 on the other hand refers to a free variable x outside of the
-- scope of this expression, and explicitely **not** to the variable defined by
-- the forall quantifier.
--
-- In general, index 0 binds to the closest binder (forall), index 1 to the
-- next, etc. If no binder is available, we call the variable free.
--
-- This can be useful in variable substitutions, where we can shift the index of
-- the variables in our expression to ensure they are not captured incorrectly.
--
-- For example, in the following substitution:
-- let e = forall y. x :@ 0
--     v = x :@ 0
--     s = y :@ 0 /\ z :@ 0
-- in e[v := s]
--
-- When performing a substitution, we would like the variable `y` in expression
-- `s` to not be captured by the `forall y.` in `e`. I.e. the following is
-- **incorrect**:
-- forall y. y :@ 0 /\ z :@ 0
--
-- Instead, we should `shift` the `y` variable in `s` upon encountering the
-- `forall y.` in `e`. Then `s` would be adjusted to:
-- s' = y :@ 1 /\ z :@ 0
--
-- If we substitute this new version `s'` in `e`, we get the intended final
-- expression:
-- forall y. y :@ 1 /\ z :@ 0
data DeBruijn a = a :@ Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Binary expression operations.
data BinOp
  = Add
  -- ^ Addition
  | Sub
  -- ^ Subtraction
  | Mul
  -- ^ Multiplication
  | Div
  -- ^ Division
  | Mod
  -- ^ Remainder
  deriving (Eq, Ord, Show)

-- | Expressions are either of type integer or array
data Expr a
  = Var !(DeBruijn a)
  -- ^ A variable
  | Const !Integer
  -- ^ An integer constant
  | BinOp !BinOp !(Expr a) !(Expr a)
  -- ^ A binary operation over expressions
  | Select !(Expr a) !(Expr a)
  -- ^ Selects a value out of an array at the given index
  --
  -- select :: array -> index -> value
  | Store !(Expr a) !(Expr a) !(Expr a)
  -- ^ Stores a value at the given index of an array, returing the modified
  -- version.
  --
  -- store :: array -> index -> value -> array
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Subable Expr where
  -- Check out the Subable typeclass for an explanation of what to implement
  -- here.
  shift = undefined

  subst' = undefined

-- | Predicates are of type boolean
data Pred a
  = !(Expr a) :==: !(Expr a)
  -- ^ Equals
  | !(Expr a) :>=: !(Expr a)
  -- ^ Greater than or equals
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Subable Pred where
  shift x = \case
    lhs :==: rhs -> go lhs :==: go rhs
    lhs :>=: rhs -> go lhs :>=: go rhs
    where
      go = shift x

  subst' var expr = \case
    lhs :==: rhs -> go lhs :==: go rhs
    lhs :>=: rhs -> go lhs :>=: go rhs
    where
      go = subst' var expr
