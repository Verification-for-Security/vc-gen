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
  { fname :: !a
  -- ^ Function name
  , fargs :: ![a]
  -- ^ Function arguments
  , fbody :: !(Statement a)
  -- ^ Function body
  , fpre :: !(Logic a)
  -- ^ Pre condition of the function
  , fpost :: !(Logic a)
  -- ^ Post condition of the function
  , fmods :: ![a]
  -- ^ The variables this function modifies
  }
  deriving (Eq, Ord, Show)

-- | Nano statement
data Statement a
  = Seq ![Statement a]
  -- ^ body0; body1; ... bodyN;
  | If !(Logic a) !(Statement a) !(Statement a)
  -- ^ If conditional body0 body1
  | While !(Logic a) !(Logic a) !(Statement a)
  -- ^ While invariant conditional body
  | Return !(Expr a)
  -- ^ Return expr
  | Assume !(Logic a)
  -- ^ Assume pred
  | Assert !(Logic a)
  -- ^ Assert pred
  | Assign !a !(Expr a)
  -- ^ x := e
  | ArrAsn !a !(Expr a) !(Expr a)
  -- ^ x[i] := e
  | AppAsn !a !a ![Expr a]
  -- ^ x := f(e0, .., eN)
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

type MonadVCGen a m = (Ord a, MonadWriter (Logic a) m, MonadReader (Info a) m, MonadState Integer m)

-- | Reader info to lookup function information.
data Info a = Info
  { ifunc :: !a
  -- ^ The current function we are checking
  , iprog :: !(Map a (Function a))
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
fresh :: MonadVCGen String m => Bool -> m (DeBruijn String)
fresh isArr = state (\s -> ((prefix <> "$fresh" <> show s) :@ 0, s + 1))
  where
    prefix = if isArr then SMT.arrPrefix else ""

-- | The return variable.
--
-- The special variable that refers to the return of a function.
result :: String
result = "$result"

-- | Havoc statement.
--
-- Erase prior knowledge we had over a variable by assigning it a fresh
-- variable. This may be used when implementing the AppAsn statement.
havoc :: MonadVCGen String m => String -> m (Statement String)
havoc var = Assign var . Var <$> fresh (SMT.isArray var)

-- | Generate verification conditions from this structure
class VCGen f where
  -- | Computes the weakest-precondition.
  vcgen :: MonadVCGen String m => f String -> Logic String -> m (Logic String)

instance VCGen Statement where
  -- | Computes the weakest-precondition.
  --
  -- Roughly, this function will be a case split over each type of statement
  -- featured in a nano program. Given a statement and the postcondition, this
  -- function should return the weakest-precondition. Consider a Hoare triple
  -- {P} s {Q}, the function receives both `s` and `Q` and should return `P`
  -- such that it is the weakest-precondition. Be sure to check out the slides
  -- if you're stuck, as they explain how to compute the weakest-precondition
  -- for the statements used in nano.
  --
  -- The tests for this function check that the verifier accepts valid programs
  -- as well that it rejects invalid programs.
  --
  -- We recommend going one by one over each statement, starting out with the
  -- simpler ones:
  -- - Seq
  -- - Assume
  -- - Assert
  -- - Assign
  -- - ArrAsn (Check out the Store expression)
  --
  -- Consider what the intended behaviour of each statement should be, and how
  -- we derive a precondition given the postcondition. You may require the
  -- substitution function (subst) for some of them.
  --
  -- After having implemented these, you can continue with implementing control-flow
  --  operations:
  -- - If
  -- - While
  --
  -- Be sure to check both branches on the if and include branching
  -- information.
  --
  -- The while statement is a lot trickier than the previous ones. In part, this
  -- is because it generates two additional constraints that are not part of
  -- the precondition:
  -- 1. What should be satisfied in order to execute the loop body once.
  -- 2. What should be satisfied once we exit the loop?
  --
  -- These additional constriants may be passed via the `tell` function. Of
  -- course, the while needs a weakest-precondition of its own as well.
  --
  -- Lastly, you may implement function contracts via the following functions:
  -- - Return
  -- - AppAsn
  --
  -- We implement return by assigning to a special variable with the
  -- corresponding name. Check out the `result` function for this special
  -- variable. To fetch the function contract of the current function, use 
  -- `currentFunc`.
  --
  -- For a function call, we need to check whether the call satisfies the
  -- function contract. One may fetch the function contract of the called
  -- function via `lookupFunc`.
  --
  -- We strongly recommend to reuse the vcgen statements for Assume, Assert,
  -- Assign and Seq instead of reimplementing these! It might also prove helpful
  -- to define a function that substitutes the function arguments for the
  -- expressions given at callsite.
  --
  -- Make sure to include the `modifies` contract, which should "erase" any
  -- predicates on the respective arrays. You may find the `havoc` function
  -- useful for this.
  vcgen stmt post = case stmt of
    Assert phi -> return $ and [phi, post]
    _ -> undefined

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
