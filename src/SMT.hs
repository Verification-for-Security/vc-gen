module SMT
  ( valid
  ) where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Expr
import Logic

import Z3.Monad (Z3)
import qualified Z3.Monad as Z3

valid :: Logic String -> IO Bool
valid phi = do
  let vs = Set.toList $ vars phi
  (res, _) <- getModel (Neg phi) vs
  return $ case res of
    Z3.Unsat -> True
    _ -> False

getModel :: Logic String -> [Expr String] -> IO (Z3.Result, Maybe String)
getModel phi vs = Z3.evalZ3 $ do
  vs' <- mapM mkVar vs
  let varMap = Map.fromList vs'
  phiz3 <- toZ3 varMap phi
  Z3.assert phiz3
  (res, maybeModel) <- Z3.getModel
  modelStr <- traverse Z3.showModel maybeModel
  return (res, modelStr)

toZ3 :: Map String Z3.AST -> Logic String -> Z3 Z3.AST
toZ3 varMap (Pred p) = toZ3Pred varMap p
toZ3 varMap (Neg e) = do
  e' <- toZ3 varMap e
  Z3.mkNot e'
toZ3 varMap (And es) = do
  es' <- mapM (toZ3 varMap) es
  Z3.mkAnd es'
toZ3 varMap (Forall e f) = do
  v <- toZ3Exp varMap (Var e)
  app <- Z3.toApp v
  f' <- toZ3 varMap f
  Z3.mkForallConst [] [app] f'

toZ3Pred :: Map String Z3.AST -> Pred String -> Z3 Z3.AST
toZ3Pred varMap (e1 :==: e2) = do
  e1' <- toZ3Exp varMap e1
  e2' <- toZ3Exp varMap e2
  Z3.mkEq e1' e2'
toZ3Pred varMap (e1 :>=: e2) = do
  e1' <- toZ3Exp varMap e1
  e2' <- toZ3Exp varMap e2
  Z3.mkGe e1' e2'
toZ3Pred varMap (e1 :<=: e2) = do
  e1' <- toZ3Exp varMap e1
  e2' <- toZ3Exp varMap e2
  Z3.mkGe e2' e1'

toZ3Exp :: Map.Map String Z3.AST -> Expr String -> Z3 Z3.AST
toZ3Exp varMap (Var v) = return $ fromJust $ Map.lookup v varMap
toZ3Exp varMap (Array a) = return $ fromJust $ Map.lookup a varMap
toZ3Exp _ (Const n) = Z3.mkInteger n
toZ3Exp varMap (BinOp Add lhs rhs)    = do
  es' <- mapM (toZ3Exp varMap) [lhs, rhs]
  Z3.mkAdd es'
toZ3Exp varMap (BinOp Sub lhs rhs)    = do
  es' <- mapM (toZ3Exp varMap) [lhs, rhs]
  Z3.mkSub es'
toZ3Exp varMap (BinOp Mul lhs rhs)    = do
  es' <- mapM (toZ3Exp varMap) [lhs, rhs]
  Z3.mkMul es'
toZ3Exp varMap (BinOp Div e1 e2)    = do
  e1' <- toZ3Exp varMap e1
  e2' <- toZ3Exp varMap e2
  Z3.mkDiv e1' e2'
toZ3Exp varMap (BinOp Mod e1 e2)    = do
  e1' <- toZ3Exp varMap e1
  e2' <- toZ3Exp varMap e2
  Z3.mkMod e1' e2'
toZ3Exp varMap (Select a i) = do
  a' <- toZ3Exp varMap a
  i' <- toZ3Exp varMap i
  Z3.mkSelect a' i'
toZ3Exp varMap (Store a i v) = do
  a' <- toZ3Exp varMap a
  i' <- toZ3Exp varMap i
  v' <- toZ3Exp varMap v
  Z3.mkStore a' i' v'

mkVar :: Expr String -> Z3.Z3 (String, Z3.AST)
mkVar (Var x) = do
  x' <- Z3.mkFreshIntVar x
  return (x, x')
mkVar (Array a) = do
  intSort <- Z3.mkIntSort
  arrSort <- Z3.mkArraySort intSort intSort
  a' <- Z3.mkFreshVar a arrSort
  return (a, a')
mkVar _ = error "Not a var"
