module Nano
  ( Nano
  , Function (..)
  , Statement (..)
  , skip
  , seq
  , VCGen (..)
  , check
  ) where

import qualified Prelude
import Prelude hiding (and, or, seq)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (foldrM)

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import qualified SMT
import Expr
import Logic

-- | A full Nano program.
type Nano a = [Function a]

-- | A function in Nano
data Function a = Function
  { fname :: a
  -- ^ Function name
  , fargs :: [a]
  -- ^ Function arguments
  , fbody :: Statement a
  -- ^ Function body
  , fpre :: Logic a
  -- ^ Pre condition of the function
  , fpost :: Logic a
  -- ^ Post condition of the function
  , fmods :: [a]
  -- ^ The variables this function modifies
  }
  deriving (Eq, Ord, Show)

-- | Nano statement
data Statement a
  = Seq [Statement a]
  -- ^ body0; body1; ... bodyN;
  | If (Logic a) (Statement a) (Statement a)
  -- ^ If conditional body0 body1
  | While (Logic a) (Logic a) (Statement a)
  -- ^ While invariant conditional body
  | Return (Expr a)
  -- ^ Return expr
  | Assume (Logic a)
  -- ^ Assume pred
  | Assert (Logic a)
  -- ^ Assert pred
  | Assign a (Expr a)
  -- ^ x := e
  | ArrAsn a (Expr a) (Expr a)
  -- ^ x[i] := e
  | AppAsn a a [Expr a]
  -- ^ x := f(e0, .., eN)
  | Havoc a
  deriving (Eq, Ord, Show)

instance Semigroup (Statement a) where
  lhs <> rhs = seq [lhs, rhs]

instance Monoid (Statement a) where
  mempty = skip

-- | Skip; essentially a No-Op
skip :: Statement a
skip = Seq []

-- | Seq (removes nested sequences)
seq :: [Statement a] -> Statement a
seq = unflatten . mconcat . (flatten <$>)
  where
    unflatten [s] = s
    unflatten s = Seq s

    flatten (Seq s) = s
    flatten s = [s]

--type MonadVCGen a m = (Ord a, MonadWriter (Logic a) m)
type MonadVCGen a m = (Ord a, MonadWriter (Logic a) m, MonadReader (Info a) m, MonadState Integer m)

-- | Reader info to lookup function information.
data Info a = Info
  { ifunc :: a
  -- ^ The current function we are checking
  , iprog :: Map a (Function a)
  -- ^ The whole program, indexed by function names
  }

-- | Lookup a function. Usefull for getting function contracts
lookupFunc :: MonadVCGen a m => a -> m (Function a)
lookupFunc name = do
  prog <- reader iprog
  return $ prog Map.! name

-- | Lookup the current function
currentFunc :: MonadVCGen a m => m (Function a)
currentFunc = reader ifunc >>= lookupFunc

-- | Get a fresh variable
fresh :: MonadVCGen String m => m String
fresh = state (\s -> ("$fresh" <> show s, s + 1))

-- | The return variable
result :: String
result = "$result"

-- | Generate verification conditions from this structure
class VCGen f where
  vcgen :: MonadVCGen String m => f String -> Logic String -> m (Logic String)

-- | Function `vcgen` implements weakest-precondition based program
-- verification, as discussed in class. It takes as input the current statement
-- and postcondtion, and returns the weakest precondition. See also slide 40
-- lecture 6.
--
-- Remember that our weakest-precondition rule for loops required us
-- to check some additional conditions on the invariants (function `vcs` in the
-- slides). In your code, you can save these additional verification conditions
-- in the background reader monad via `tell`.
--
-- These additional conditions will be checked together with the weakest
-- precondition produced by your implementation.
--
-- We recommend first implementing the basic algebra nano commands, such as Seq,
-- If Assume, Assert, Assign and ArrAsn, after which you can implement While.
--
-- If you have done all these steps, then you can start with the function
-- contract statements: Havoc, Return and AppAsn.
--
-- Of this, Havoc is probably the easiest, followed by Return. Note that Havoc
-- is never generated from the parser directly. It can however be usefull for
-- AppAsn.
--
-- For Return and AppAsn, we use the 'result' function to name the output of
-- a function.
--
-- You can use 'fresh' to generate a fresh variable name.
instance VCGen Statement where
  vcgen = undefined

instance VCGen Function where
  vcgen func post' = do
    let pre = Assume $ fpre func
    let body = fbody func
    let post = Assert $ fpost func
    let stmt = seq [pre, body, post]
    vcgen stmt post'

-- | Check whether a given nano program is valid.
check :: Nano String -> IO Bool
check nano = do
  let progmap = Map.fromList $ (\f -> (fname f, f)) <$> nano
  let runner fun = runWriter . flip runReaderT (Info (fname fun) progmap) . flip evalStateT 0
  let check' fun = let (pre, vcs) = runner fun $ vcgen fun true in pre <> vcs
  let vcs = check' <$> nano
  valid <- mapM SMT.valid vcs
  return $ Prelude.and valid
