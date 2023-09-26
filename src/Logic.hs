module Logic
  ( Logic (..)
  , neg
  , true
  , false
  , and
  , or
  , exists
  , implies
  , iff
  ) where

import Expr
import Prelude hiding (and, or)

import qualified Data.Set as Set

-- | The constraints that we generate from Nano programs.
data Logic a
  = Pred (Pred a)
  -- ^ Predicate
  | Neg (Logic a)
  -- ^ Negation
  | And [Logic a]
  -- ^ Conjunction
  | Forall a (Logic a)
  -- ^ Universal quantification
  deriving (Eq, Ord, Show)

instance Semigroup (Logic a) where
  lhs <> rhs = and [lhs, rhs]

instance Monoid (Logic a) where
  mempty = true

-- | Negation (removes double negatives)
neg :: Logic a -> Logic a
neg (Neg l) = l
neg l = Neg l

-- | True
true :: Logic a
true = And []

-- | False
false :: Logic a
false = Neg true

-- | Conjunction (removes nested conjuncts)
and :: [Logic a] -> Logic a
and = unflatten . mconcat . (flatten <$>)
  where
    unflatten [s] = s
    unflatten s = And s

    flatten (And s) = s
    flatten s = [s]

-- | Disjunction
or :: [Logic a] -> Logic a
or = neg . and . (neg <$>)

-- | Existential quantification
exists :: a -> Logic a -> Logic a
exists x = neg . Forall x . neg

-- | Implication
implies :: Logic a -> Logic a -> Logic a
implies lhs rhs = neg $ and [lhs, neg rhs]

-- | Bi-Implication
iff :: Logic a -> Logic a -> Logic a
iff lhs rhs = and [implies lhs rhs, implies rhs lhs]

instance Vars Logic where
  vars = \case
    Pred p -> vars p
    Neg l -> vars l
    And l -> mconcat $ map vars l
    Forall x l -> Set.union (vars l) (vars $ Var x)

instance Subable Logic where
  subst = undefined
