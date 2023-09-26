module LogicSpec
  ( rubric
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import Prelude hiding (or)
import Expr (Subable (subst), Expr (..), Pred (..), BinOp (..))
import Logic (Logic (..), true, false, implies, or)

rubric :: Rubric
rubric = do
  let num = Const 4
  let var = "subst"
  let var' = Var var
  let unused = "unused"

  let add = BinOp Add
  let sub = BinOp Sub
  let mul = BinOp Mul

  let e0 e = num `add` e `add` Var "x" `add` e
  let e1 e = e0 e `mul` num `mul` e `mul` (Const 0 `sub` Var "y")
  let e2 e = e0 e `sub` Var "z" `sub` e1 e `sub` Var "x"

  criterion "Expr" (1/3) . passOrFail $ do
    it "subst" $ do
      -- Var not present
      subst unused num (e0 var') @?= e0 var'
      subst unused num (e1 var') @?= e1 var'
      subst unused num (e2 var') @?= e2 var'

      -- Actual substitution
      subst var num (e0 var') @?= e0 num
      subst var num (e1 var') @?= e1 num
      subst var num (e2 var') @?= e2 num
      subst var (e0 num) (e2 var') @?= (e2 . e0) num
      subst var (e1 num) (e2 var') @?= (e2 . e1) num

  let p0 e = e0 e :>=: e1 e
  let p1 e = e1 e :==: e2 e

  criterion "Predicate" (1/3) . passOrFail $ do
    it "subst" $ do
      subst var num (p0 var') @?= p0 num
      subst var num (p1 var') @?= p1 num
      subst var (e0 num) (p1 var') @?= (p1 . e0) num 

  let b0 e = And [Neg . Pred $ e1 e :>=: e, Pred $ e0 e :==: num]
  let b1 e = or [Neg . Pred $ num :<=: e2 e, true, And [b0 e], or [b0 e]]
  let b2 e = Forall "$v" $ implies false $ implies (b0 e) (b1 e)

  criterion "Logic" (1/3) . passOrFail $ do
    it "subst" $ do
      -- Var not present
      subst unused num (b0 $ e2 var') @?= (b0 . e2) var'
      subst unused num (b1 $ e2 var') @?= (b1 . e2) var'
      subst unused num (b2 $ e2 var') @?= (b2 . e2) var'

      -- Actual substitution
      subst var num (b0 $ e2 var') @?= (b0 . e2) num
      subst var num (b1 $ e2 var') @?= (b1 . e2) num
      subst var num (b2 $ e2 var') @?= (b2 . e2) num
      subst var (e1 num) (b2 $ e2 var') @?= (b2 . e2 . e1) num
      subst var (e2 num) (b2 $ e2 var') @?= (b2 . e2 . e2) num
