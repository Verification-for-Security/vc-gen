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
--
-- Again, use the patterns below!
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
-- - Remaining expressions should become predicates (if possible)
--
-- Again, use the patterns below!
logic :: MonadNano String m => JS.Expression a -> m (Logic String)
logic = undefined

-- | Converts JS into Nano expressions of type Bool
--
-- This should convert the following subset of JS into Nano expressions:
-- - All (strict) (in)equalities
--
-- Notice how we return Logic here, this is because we express some of the
-- operations via a negation of a predicate.
--
-- Again, use the patterns below!
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
-- Below, you will find a bunch of patterns which you can use to implement
-- the parser. You can essentially implement the functions above with just
-- these patterns, with the exception of having to match on the JS operators.
--
-- If you're curious about what these patterns are exactly, you can look up the
-- PatternSynonyms Haskell pragma.
--
-- You can use these patterns as follows:
-- expr (Variable var) = ...
--
-- Here, 'var' will be of type String, as dictated by the pattern.
--
-- Note that you will have to us every pattern at least once (and some multiple
-- times), unless stated otherwise.
--
-- If you miss a case that you should parse, check out what the JavaScript 
-- parser will produce by running it separately. This way, you could find
-- the culprit expression.
expr :: MonadNano String m => JS.Expression a -> m (Expr String)
expr = undefined

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

pattern ArrayIndex :: String -> JS.Expression a -> JS.Expression a
pattern ArrayIndex array index <- JS.BracketRef _ (Variable array) index

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
