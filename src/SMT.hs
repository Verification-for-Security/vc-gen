{-# OPTIONS_GHC -Wno-orphans #-}
module SMT
  ( valid
  , arrPrefix
  , isArray
  ) where

import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map

import Expr
import Logic

import Z3.Monad (MonadZ3)
import qualified Z3.Monad as Z3

import Control.Monad.State

arrPrefix :: String
arrPrefix = "arr_"

isArray :: String -> Bool
isArray = isPrefixOf arrPrefix

valid :: Logic String -> IO Bool
valid phi = do
  (res, _) <- getModel (Neg phi)
  return $ case res of
    Z3.Unsat -> True
    _ -> False

getModel :: Logic String -> IO (Z3.Result, Maybe String)
getModel phi = Z3.evalZ3 $ do
  phi' <- z3Vars >=> toZ3 $ phi
  Z3.assert phi'
  (res, maybeModel) <- Z3.getModel
  modelStr <- traverse Z3.showModel maybeModel
  return (res, modelStr)

mkFresh :: MonadZ3 m => String -> m Z3.AST
mkFresh name = do
  sort <- mkSort name
  Z3.mkFreshVar name sort

mkSort :: MonadZ3 m => String -> m Z3.Sort
mkSort name
  | "arr_" `isPrefixOf` name = do
    intSort <- Z3.mkIntSort
    Z3.mkArraySort intSort intSort
  | otherwise = Z3.mkIntSort

type MonadRename m = (MonadState Free m, MonadZ3 m)

type Scope = Map String [Z3.AST]
type Free = Map String Z3.AST

instance MonadZ3 m => MonadZ3 (StateT s m) where
  getSolver = lift Z3.getSolver
  getContext = lift Z3.getContext

class Z3Vars f where
  z3Vars' :: MonadState Free m => MonadZ3 m => Scope -> f String -> m (f Z3.AST)

  z3Vars :: MonadZ3 m => f String -> m (f Z3.AST)
  z3Vars = flip evalStateT mempty . z3Vars' mempty

lookupVar :: MonadRename m => Scope -> DeBruijn String -> m Z3.AST
lookupVar scope (x:@i) = maybe free return local
  where
    local = case Map.lookup x scope of
      Just vars -> vars !? i
      Nothing -> Nothing

    free = get >>= \fv -> case Map.lookup x fv of
      Just v -> return v
      Nothing -> do
        fresh <- mkFresh x
        let fv' = Map.insert x fresh fv
        put fv'
        return fresh

extendScope :: MonadRename m => String -> Scope -> m (Z3.AST, Scope)
extendScope x scope = do
  fresh <- mkFresh x
  let scope' = Map.insertWith (<>) x [fresh] scope
  return (fresh, scope')

instance Z3Vars Expr where
  z3Vars' scope = \case
    Var v -> do
      x' <- lookupVar scope v
      return $ Var (x':@0)
    Const i -> return $ Const i
    BinOp op lhs rhs -> do
      lhs' <- go lhs
      rhs' <- go rhs
      return $ BinOp op lhs' rhs'
    Select array index -> do
      array' <- go array
      index' <- go index
      return $ Select array' index'
    Store array index assign -> do
      array' <- go array
      index' <- go index
      assign' <- go assign
      return $ Store array' index' assign'
    where
      go = z3Vars' scope

instance Z3Vars Pred where
  z3Vars' scope = \case
    lhs :==: rhs -> do
      lhs' <- go lhs
      rhs' <- go rhs
      return $ lhs' :==: rhs'
    lhs :>=: rhs -> do
      lhs' <- go lhs
      rhs' <- go rhs
      return $ lhs' :>=: rhs'
    where
      go = z3Vars' scope

instance Z3Vars Logic where
  z3Vars' scope = \case
    Pred p -> Pred <$> z3Vars' scope p
    Neg l -> Neg <$> z3Vars' scope l
    And ls -> And <$> mapM (z3Vars' scope) ls
    Forall x l -> do
      (x', scope') <- extendScope x scope
      l' <- z3Vars' scope' l
      return $ Forall x' l'

class ToZ3 f where
  toZ3 :: MonadZ3 m => f Z3.AST -> m Z3.AST

instance ToZ3 Expr where
  toZ3 = \case
    Var (x:@_) -> return x
    Const i -> Z3.mkInteger i
    BinOp op lhs rhs -> do
      let binary f x y = f [x, y]
      let op' = case op of
            Add -> binary Z3.mkAdd
            Sub -> binary Z3.mkSub
            Mul -> binary Z3.mkMul
            Div -> Z3.mkDiv
            Mod -> Z3.mkMod
      lhs' <- toZ3 lhs
      rhs' <- toZ3 rhs
      op' lhs' rhs'
    Select a i -> do
      a' <- toZ3 a
      i' <- toZ3 i
      Z3.mkSelect a' i'
    Store a i v -> do
      a' <- toZ3 a
      i' <- toZ3 i
      v' <- toZ3 v
      Z3.mkStore a' i' v'

instance ToZ3 Pred where
  toZ3 = \case
    e1 :==: e2 -> do
      e1' <- toZ3 e1
      e2' <- toZ3 e2
      Z3.mkEq e1' e2'
    e1 :>=: e2 -> do
      e1' <- toZ3 e1
      e2' <- toZ3 e2
      Z3.mkGe e1' e2'

instance ToZ3 Logic where
  toZ3 = \case
    Pred p -> toZ3 p
    Neg e -> do
      e' <- toZ3 e
      Z3.mkNot e'
    And es -> do
      es' <- mapM toZ3 es
      Z3.mkAnd es'
    Forall x l -> do
      app <- Z3.toApp x
      l' <- toZ3 l
      Z3.mkForallConst [] [app] l'

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
     0 -> Just x
     _ -> r (k-1)) (const Nothing) xs n
