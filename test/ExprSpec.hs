module ExprSpec
  ( rubric
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import Prelude hiding (or)
import Expr (Subable (..), Expr (..), Pred (..), BinOp (..), DeBruijn (..))
import Logic (Logic (..), true, false, implies, or)

rubric :: Rubric
rubric = do
  let num = Const 4
  let var = "subst"

  let var0 = var :@ 0
  let var1 = var :@ 1
  let var2 = var :@ 2
  let var0' = Var var0
  let var1' = Var var1
  let var2' = Var var2

  let unused = "unused"

  let add = BinOp Add
  let sub = BinOp Sub
  let mul = BinOp Mul

  let e0 e = num `add` e `add` Var ("x" :@ 0) `add` e
  let e1 e = e0 e `mul` num `mul` e `mul` (Const 0 `sub` Var ("y" :@ 0))
  let e2 e = Select (e0 e) (Var ("z" :@ 0)) `sub` Store (e1 e) (Var ("x" :@ 0)) (Const 4)

  criterion "instance Subable Expr" (1/3) $ do
    criterion "shift" (1/2) . passOrFail $ do

      it "doesn't shift bound variables" $ do
        -- Although we don't truly have bound variables here, we would only
        -- shift with a scope > 0 if we passd a binder during traversal. Hence,
        -- we emulate that by directly passing in a bumped shift.
        shift var1 var0' @?= var0'
        shift var1 (e0 var0') @?= e0 var0'
        shift var2 (e2 var1') @?= e2 var1'

      it "shifts free variables" $ do
        shift var1 var1' @?= var2'
        shift var0 var1' @?= var2'
        shift var0 (e0 var0') @?= e0 var1'
        shift var0 (e0 var1') @?= e0 var2'
        shift var1 (e2 var1') @?= e2 var2'
        shift var1 (e2 var1' `add` e2 var0') @?= e2 var2' `add` e2 var0'

    criterion "subst'" (1/2) . passOrFail $ do
      it "substitutes matching variables" $ do
        let x = Var $ "x" :@ 0
        subst' var1 var0' var1' @?= var0'
        subst' var0 var1' var0' @?= var1'
        subst' var0 var2' var0' @?= var2'
        subst' var0 x var0' @?= x

      it "traverses the expression" $ do
        -- Var not present
        subst unused num (e0 var0') @?= e0 var0'
        subst unused num (e1 var0') @?= e1 var0'
        subst unused num (e2 var0') @?= e2 var0'

        -- Actual substitution
        subst' var0 num (e0 var0') @?= e0 num
        subst' var0 num (e1 var0') @?= e1 num
        subst' var0 num (e2 var0') @?= e2 num
        subst' var0 (e0 num) (e2 var0') @?= (e2 . e0) num
        subst' var0 (e1 num) (e2 var0') @?= (e2 . e1) num

        subst' var1 num (e0 var1') @?= e0 num
        subst' var1 num (e1 var1') @?= e1 num
        subst' var1 num (e2 var1') @?= e2 num
        subst' var1 (e0 num) (e2 var1') @?= (e2 . e0) num
        subst' var1 (e1 num) (e2 var1') @?= (e2 . e1) num

  let p0 e = e0 e :>=: e1 e
  let p1 e = e1 e :==: e2 e
  let p2 e = e :>=: e 

  criterion "instance Subable Pred" (1/3) $ do
    criterion "shift" (1/2) . passOrFail $ do
      it "shifts both sides of the (in)equality" $ do
        shift var0 (p2 var0') @?= p2 var1'
        shift var1 (p2 var0') @?= p2 var0'

        shift var0 (p0 var0') @?= p0 var1'
        shift var1 (p0 var0') @?= p0 var0'
        shift var0 (p0 var1') @?= p0 var2'
        shift var0 (p0 $ e0 var1') @?= p0 (e0 var2')

        shift var0 (p1 var0') @?= p1 var1'
        shift var1 (p1 var0') @?= p1 var0'
        shift var0 (p1 var1') @?= p1 var2'
        shift var0 (p1 $ e0 var1') @?= p1 (e0 var2')

    criterion "subst'" (1/2) . passOrFail $ do
      it "substitutes both sides of the (in)equality" $ do
        subst' var1 num (p2 var1') @?= p2 num
        subst' var1 num (p2 var2') @?= p2 var2'

        subst' var0 num (p0 var0') @?= p0 num
        subst' var0 num (p1 var0') @?= p1 num
        subst' var0 (e0 var1') (p1 var0') @?= (p1 . e0) var1'

        subst' var1 num (p0 var1') @?= p0 num
        subst' var1 num (p1 var1') @?= p1 num
        subst' var1 (e0 var1') (p1 var1') @?= (p1 . e0) var1'

  let b0 e = And [Neg . Pred $ e1 e :>=: e, Pred $ e0 e :==: num]
  let b1 e = or [Neg . Pred $ num :>=: e2 e, true, And [b0 e], or [b0 e]]
  let b2 q e = Forall q $ implies false $ implies (b0 e) (b1 e)
  let b3 q b = Forall q $ And [Pred $ Var ("x" :@ 0) :==: Const 0, b]
  let p2' e = Pred $ p2 e

  criterion "instance Subable Logic" (1/3) $ do
    criterion "shift" (1/2) . passOrFail $ do
      it "shifts free variables" $ do
        shift var0 (p2' var0') @?= p2' var1'
        shift var0 (b0 $ e2 var0') @?= (b0 . e2) var1'
        shift var0 (b1 var0') @?= b1 var1'
        shift var0 (b2 var var1') @?= b2 var var2'

      it "doesn't shift bound variables" $ do
        shift var1 (p2' var0') @?= p2' var0'
        shift var0 (b3 var $ p2' var0') @?= b3 var (p2' var0')
        shift var0 (b2 var var0') @?= b2 var var0'
        shift var0 (b2 var $ e2 var0') @?= b2 var (e2 var0')
        shift var0 (b3 var . b3 var . p2' $ var0') @?= (b3 var . b3 var . p2') var0'

    criterion "subst'" (1/2) . passOrFail $ do
      it "does basic substitution" $ do
        -- Var not present
        subst unused num (b0 $ e2 var0') @?= (b0 . e2) var0'
        subst unused num (b1 $ e2 var1') @?= (b1 . e2) var1'
        subst unused num (b2 var $ e2 var2') @?= (b2 var . e2) var2'

        -- Actual substitution
        subst var num (b0 $ e2 var0') @?= (b0 . e2) num
        subst var num (b1 $ e2 var0') @?= (b1 . e2) num
        subst var num (b2 unused $ e2 var0') @?= (b2 unused . e2) num
        subst var (e1 num) (b2 unused $ e2 var0') @?= (b2 unused . e2 . e1) num
        subst var (e2 num) (b2 unused $ e2 var0') @?= (b2 unused . e2 . e2) num

      let b4 q e = Forall q (Pred $ e :==: Var ("y" :@ 0))

      it "substitutes correct variable w.r.t. index" $ do
        let x = Var $ "x" :@ 0

        subst var x (b4 var var0') @?= b4 var var0'
        subst var x (b4 var var1') @?= b4 var x

      it "avoids captures via shift" $ do
        subst var var0' (b4 var var1') @?= b4 var var1'
        subst var var1' (b4 var var1') @?= b4 var var2'

        let y = var1' `add` var0'
        let y' = var2' `add` var1'
        subst var y (b4 var var1') @?= b4 var y'
