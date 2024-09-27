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
  { invariant :: !(Logic a)
  , require :: !(Logic a)
  , ensure :: !(Logic a)
  , modifies :: ![a]
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
-- - Assignments, Nano has three types of assignments:
--   - x := f(e0, .., eN)
--   - x := expr
--   - arr[i] := expr
-- - Variable declaration (only when it assigns a value)
-- - Block statement
-- - If statement (with and without else)
-- - While statement (check `scopeInv`)
-- 
-- - Specialise functions with the names "assume" and "assert" to their
--   respective Nano statement.
--
-- - The calls "requires", "ensures" and "modifies" strengthen the contract of
--   the function these statements occur in. We track adjustments to the
--   function contract via the calls `addRequire`, `addEnsure` and `addModifies`
--   respectively. The statement itself does not do anything else, thus a skip
--   suffices as a return.
--
-- - In a similar fashion, the call "invariant" tracks predicates for the while
--   statement it occurs in. A call to `addInvariant` adds invariants that
--   can later be retrieved by the call to `scopeInv`.
-- - `empty` for remaining patterns.
statement :: MonadNano String m => JS.Statement a -> m (Statement String)
statement _ = empty

-- | Helper function to scope invariant fetching to a block.
--
-- You may pass in an operation that parses a statement, usually the body of a
-- while loop. This will collect all the invariants that were added during the
-- parse using `addInvariant`.
scopeInv :: MonadNano a m => m (Statement a) -> m (Statement a, Logic a)
scopeInv m = do
  outer <- state $ \cons -> (invariant cons, cons { invariant = true })
  result <- m
  inv <- state $ \cons -> (invariant cons, cons { invariant = outer })
  return (result, inv)

-- | Helper function to add an invariant to the upper while.
addInvariant :: MonadNano a m => Logic a -> m ()
addInvariant l = modify (mempty { invariant = l } <>)

-- | Helper function to add a require to the upper function.
addRequire :: MonadNano a m => Logic a -> m ()
addRequire l = modify (mempty { require = l } <>)

-- | Helper function to add an ensure to the upper function.
addEnsure :: MonadNano a m => Logic a -> m ()
addEnsure l = modify (mempty { ensure = l } <>)

-- | Helper function to add a modifies to the upper function.
addModifies :: MonadNano a m => a -> m ()
addModifies x = modify (mempty { modifies = [x] } <>)

-- | Converts JS into Nano logic.
--
-- This should convert the following subset of JS into Nano expressions:
-- - Boolean literals
-- - Conjuncts and Disjuncts
-- - Negation
-- - Functions called "forall" or "exists" with two arguments (of which the
--   first a variable) into its respective quantifier. 
-- - Remaining expressions should be parsed as predicates
--
-- Note; make sure that remaining infix expressions besides conjuncts and
-- disjuncts get passed down to the predicate function call.
--
-- Again, use the patterns below!
logic :: (Monad m, Alternative m) => JS.Expression a -> m (Logic String)
logic _ = empty

-- | Converts JS into Nano expressions of type Bool
--
-- This should convert the following subset of JS into Nano expressions:
-- - All (strict) (in)equalities (i.e. ==, !=, >=, <=, >, <)
-- - `empty` for remaining patterns.
--
-- Notice that the return value of this function is not actually of type Pred,
-- but of type Logic. This is because we express some of the operations via a
-- negation of a predicate. I.e. `x < y` is equivalent to `~(x >= y)`.
-- The operands are expressions and should be parsed as such.
--
-- Remember to use the patterns we defined below!
predicate :: (Monad m, Alternative m) => JS.Expression a -> m (Logic String)
predicate _ = empty

-- | Converts JS into Nano expressions of type Int.
--
-- This should convert the following subset of JS into Nano expressions:
-- - Integer literals
-- - Variables
-- - Array indexing
-- - Binary arithmetic (only those supported by Nano)
-- - Unary minus
-- - `empty` for remaining patterns
--
-- As nano accepts a subset of the javascript language, the remaining cases may
-- just be caught by a call to `empty`. Alternatively, when debugging, one can
-- use a call to `error` which can be used to print the uncaught expression!
--
-- While you can look up the types of a JS Expression in their docs, we
-- recommend you use the patterns defined at the bottom of this document! You
-- can essentially implement the functions above with just these patterns, with
-- the exception of having to match on the binary JS operators (e.g. OpAdd).
--
-- You can use these patterns as follows:
-- expr (Variable var) = ...
--
-- Here, 'var' will be of type String, as dictated by the pattern.
--
-- If you're curious about what these patterns are exactly, you can look up the
-- PatternSynonyms Haskell pragma.
expr :: (Monad m, Alternative m) => JS.Expression a -> m (Expr String)
expr = \case
  Variable name -> return $ Var (name :@ 0)
  _ -> empty

pattern Variable :: String -> JS.Expression a
pattern Variable x <- JS.VarRef _ (JS.Id _ x)

pattern Int :: Int -> JS.Expression a
pattern Int i <- JS.IntLit _ i

pattern Bool :: Bool -> JS.Expression a
pattern Bool b <- JS.BoolLit _ b

pattern Minus :: JS.Expression a -> JS.Expression a
pattern Minus e <- JS.PrefixExpr _ JS.PrefixMinus e

pattern Negate :: JS.Expression a -> JS.Expression a
pattern Negate e <- JS.PrefixExpr _ JS.PrefixLNot e

pattern ArrayIndex :: JS.Expression a -> JS.Expression a -> JS.Expression a
pattern ArrayIndex array index <- JS.BracketRef _ array index

pattern InfixExpr :: JS.Expression a -> JS.InfixOp -> JS.Expression a -> JS.Expression a
pattern InfixExpr lhs op rhs <- JS.InfixExpr _ op lhs rhs

pattern Call :: String -> [JS.Expression a] -> JS.Expression a
pattern Call name arguments <- JS.CallExpr _ (Variable name) arguments

pattern CallStmt :: String -> [JS.Expression a] -> JS.Statement a
pattern CallStmt name arguments <- JS.ExprStmt _ (Call name arguments)

pattern WhileStmt :: JS.Expression a -> JS.Statement a -> JS.Statement a
pattern WhileStmt conditional body <- JS.WhileStmt _ conditional body

pattern IfStmt :: JS.Expression a -> JS.Statement a -> JS.Statement a -> JS.Statement a
pattern IfStmt conditional body0 body1 <- JS.IfStmt _ conditional body0 body1

pattern IfSingleStmt :: JS.Expression a -> JS.Statement a -> JS.Statement a
pattern IfSingleStmt conditional body <- JS.IfSingleStmt _ conditional body

pattern BlockStmt :: [JS.Statement a] -> JS.Statement a
pattern BlockStmt body <- JS.BlockStmt _ body

pattern EmptyStmt :: JS.Statement a
pattern EmptyStmt <- JS.EmptyStmt _

pattern ReturnStmt :: JS.Expression a -> JS.Statement a
pattern ReturnStmt expr <- JS.ReturnStmt _ (Just expr)

-- | This is a helper for the other assign statements, you do not have to use
-- this directly.
pattern AssignStmt' :: JS.LValue a -> JS.Expression a -> JS.Statement a
pattern AssignStmt' lhs rhs <- JS.ExprStmt _ (JS.AssignExpr _ JS.OpAssign lhs rhs)

-- | You still have to distinguish between an expression or function call on
-- the rhs when using this pattern.
pattern AssignStmt :: String -> JS.Expression a -> JS.Statement a
pattern AssignStmt var rhs <- AssignStmt' (JS.LVar _ var) rhs

pattern ArrAsnStmt :: String -> JS.Expression a -> JS.Expression a -> JS.Statement a
pattern ArrAsnStmt array index rhs <- AssignStmt' (JS.LBracket _ (Variable array) index) rhs

pattern DeclStmt :: [JS.VarDecl a] -> JS.Statement a
pattern DeclStmt statements <- JS.VarDeclStmt _ statements

pattern Decl :: String -> JS.Expression a -> JS.VarDecl a
pattern Decl var expr <- JS.VarDecl _ (JS.Id _ var) (Just expr)
