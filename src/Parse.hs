module Parse
  ( Constraints
  , nano
  , function
  , statement
  , logic
  , predicate
  , expr
  ) where

import qualified Language.ECMAScript3.Syntax as JS
import qualified Language.ECMAScript3.Parser as JS
import Data.Composition

import Control.Monad.State
import Control.Applicative (empty, Alternative)
import Prelude hiding (seq, and, or)

import Expr
import Logic
import Nano

data Constraints a = Constraints
  { invariant :: Logic a
  , require :: Logic a
  , ensure :: Logic a
  , modifies :: [a]
  }
  deriving (Show, Eq, Ord)

instance Semigroup (Constraints a) where
  Constraints i r e m <> Constraints i' r' e' m' 
    = Constraints (i <> i') (r <> r') (e <> e') (m <> m')

instance Monoid (Constraints a) where
  mempty = Constraints mempty mempty mempty mempty

type MonadNano a m = (MonadState (Constraints a) m, Alternative m)

-- | Parses Javascript and converts it into Nano.
nano :: String -> IO (Maybe (Nano String))
nano path = do
  JS.Script _ stmts <- JS.parseFromFile path
  return $ mapM function stmts

-- | Converts a JS function statement into a function.
function :: JS.Statement a -> Maybe (Function String)
function (JS.FunctionStmt _ (JS.Id _ name) args body) = do
  (body', constraints) <- flip runStateT mempty $ mapM statement body
  let args' = (\(JS.Id _ v) -> v) <$> args
  return $ Function 
    { fname = name
    , fargs = args'
    , fbody = seq body'
    , fpre = require constraints
    , fpost = ensure constraints
    , fmods = modifies constraints
    }
function _ = empty

-- | Converts JS into Nano.
--
-- This should convert the following subset of JS into Nano statements:
-- - Empty statement
-- - Return
-- - Assignment (both 'x := expr' and 'arr[i] := expr')
-- - Variable declaration (only when it assigns a value)
-- - Block statement
-- - If statement (with and without else)
-- - While statement (check `scopeInv`)
-- - Specialise function with the names "assume", "assert", "invariant", 
--   "requires" and "ensures". To perform appropriate actions.
statement :: MonadNano String m => JS.Statement a -> m (Statement String)
statement = undefined

-- | Helper function to scope invariant fetching to a block.
scopeInv :: MonadNano String m => m a -> m (a, Logic String)
scopeInv m = do
  outer <- state $ \cons -> (invariant cons, cons { invariant = true })
  result <- m
  inv <- state $ \cons -> (invariant cons, cons { invariant = outer })
  return (result, inv)

-- | Helper function to add an invariant to the upper while.
addInvariant :: MonadNano a m => Logic a -> m ()
addInvariant l = modify (mempty { invariant = l } <>)

-- | Helper function to add an require to the upper function.
addRequire :: MonadNano a m => Logic a -> m ()
addRequire l = modify (mempty { require = l } <>)

-- | Helper function to add an ensure to the upper function.
addEnsure :: MonadNano a m => Logic a -> m ()
addEnsure l = modify (mempty { ensure = l } <>)

-- | Converts JS into Nano logic.
--
-- This should convert the following subset of JS into Nano expressions:
-- - Boolean literals
-- - Conjuncts and Disjuncts
-- - Negation
-- - Functions called "forall" or "exists" with two arguments (of which the
--   first a variable) into its respective quantifier. 
--   Hint: check 'CallExpr'
-- - Remaining expressions should become predicates (if possible)
logic :: MonadNano String m => JS.Expression a -> m (Logic String)
logic = undefined

-- | Converts JS into Nano expressions of type Bool
--
-- This should convert the following subset of JS into Nano expressions:
-- - All (strict) (in)equalities
--
-- Notice how we return Logic here, this is because we express some of the
-- operations via a negation of a predicate.
predicate :: MonadNano String m => JS.Expression a -> m (Logic String)
predicate = undefined

-- | Converts JS into Nano expressions of type Int
--
-- This should convert the following subset of JS into Nano expressions:
-- - Integer literals
-- - Variables
-- - Array indexing (the array itself may be just a variable)
-- - Binary arithmetic
-- - Unary minus
--
-- You can look up the types of a JS.Expression in their docs.
-- For an example of a variable pattern match, check out the 'Variable'
-- pattern below. You are free to add more patterns like this, or just straight
-- up pattern match against the code like you would normally.
--
-- Note that all these expressions contain location information, you may just
-- discard this with a '_' in your pattern match.
--
-- If you miss a case that you should parse, check out what the JavaScript 
-- parser will produce by running it separately. This way, you could find
-- the culprit expression.
expr :: MonadNano String m => JS.Expression a -> m (Expr String)
expr = undefined

-- | You can use this to pattern match on a variable.
-- For more info on this, search for the PatternSynonyms language pragma.
--
-- Feel free to add more patterns if you wish!
pattern Variable :: String -> JS.Expression a
pattern Variable x <- JS.VarRef _ (JS.Id _ x)
