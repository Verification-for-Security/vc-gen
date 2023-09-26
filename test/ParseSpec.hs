module ParseSpec
  ( rubric
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import Language.ECMAScript3.Parser (parse, expression)

import Control.Monad.State

import Util

import Prelude hiding (seq, and, or)

import Expr
import Logic
import Nano hiding (check)
import qualified Parse

rubric :: Rubric
rubric = do
  let add = BinOp Add
  let sub = BinOp Sub
  let mul = BinOp Mul

  let unwrap (Right x) = x
      unwrap (Left  x) = error . show $ x

  -- Nano parsers
  let transform ty = flip evalStateT mempty . ty . unwrap. parse expression ""
  let expr = transform Parse.expr
  let predicate = transform Parse.predicate
  let logic = transform Parse.logic

  -- Expressions
  let e0 = "x + 4 * 23 - y"
  let e1 = "x * (y + 15 * 23) * (-3)"
  let e2 = "x[i] - y - z - 5 * 2"
  let e3 = "x + (y[i] * 25) + 4 + z"

  -- In syntax tree format
  let e0' = sub (add (Var "x") (mul (Const 4) (Const 23))) (Var "y")
  let e1' = mul (mul (Var "x") (add (Var "y") (mul (Const 15) (Const 23)))) (sub (Const 0) (Const 3))
  let e2' = sub (sub (sub (Select (Array "x") (Var "i")) (Var "y")) (Var "z")) (mul (Const 5) (Const 2))
  let e3' = add (add (add (Var "x") (mul (Select (Array "y") (Var "i")) (Const 25))) (Const 4)) (Var "z")

  passes "expr" (1/6) $ do
    expr e0 @?= Just e0'
    expr e1 @?= Just e1'
    expr e2 @?= Just e2'
    expr e3 @?= Just e3'

  -- Boolean Expressions
  let p0 = concat [e0, ">=", e1]
  let p1 = concat [e3, "!=", e2]
  let p2 = concat [e0, ">", e3]
  let p3 = concat [e2, "==", e1]

  -- In syntax tree format
  let p0' = Pred $ e0' :>=: e1'
  let p1' = Neg . Pred $ e3' :==: e2'
  let p2' = Neg . Pred $ e0' :<=: e3'
  let p3' = Pred $ e2' :==: e1'
 
  passes "predicate" (1/6) $ do
    predicate p0 @?= Just p0'
    predicate p1 @?= Just p1'
    predicate p2 @?= Just p2'
    predicate p3 @?= Just p3'

  -- Boolean Expressions
  let b0 = concat [e0, ">=", e1, "&& !true"]
  let b1 = concat [e3, "!=", e2, "||", b0]
  let b2 = concat ["false && (", b1, ") &&", e0, "<=", e3]
  let b3 = concat [e2, "==", e1, "||", b2, "||", "true"]
  let b4 = "forall(x, x == 4)"
  let b5 = "exists(x, y >= x)"

  -- In syntax tree format
  let b0' = Pred (e0' :>=: e1') <> neg true
  let b1' = or [Neg . Pred $ e3' :==: e2', b0']
  let b2' = and [false, b1', Pred $ e0' :<=: e3']
  let b3' = or [Pred $ e2' :==: e1', b2', true]
  let b4' = Forall "x" . Pred $ Var "x" :==: Const 4
  let b5' = exists "x" . Pred $ Var "y" :>=: Var "x"
 
  passes "logic" (1/6) $ do
    logic b0 @?= Just b0'
    logic b1 @?= Just b1'
    logic b2 @?= Just b2'
    logic b3 @?= Just b3'
    logic b4 @?= Just b4'
    logic b5 @?= Just b5'

  criterion "statement" (3/6) . distribute $ do
    let nano file = (normalize <$>) <$> Parse.nano file

    let check file func = dpasses ("correctly parses " <> file) $ do
          nano file `shouldReturn` return func

    check "programs/pos/abs.js"
      [ Function
        { fname = "abs"
        , fargs = ["x"]
        , fpre = true
        , fpost = true
        , fmods = []
        , fbody = Seq
          [ Assign "res" (Const 0)
          , If (Pred $ Var "x" :>=: Const 1)
               (Assign "res" (Var "x"))
               (Assign "res" (sub (Const 0) (Var "x")))
          , Assert . Pred $ Var "res" :>=: Const 0
          ]
        }
      ]

    check "programs/pos/while5.js"
      [ Function
        { fname = "foo"
        , fargs = []
        , fpre = true
        , fpost = true
        , fmods = []
        , fbody = Seq
          [ Assign "x" $ Const 0
          , While (Pred $ Var "x" :<=: Const 6)
                  (Pred $ Var "x" :<=: Const 5)
                  (Assign "x" (add (Var "x") (Const 1)))
          , Assert . Pred $ Var "x" :==: Const 6
          ]
        }
      ]

    check "programs/pos/ifassert.js"
      [ Function
        { fname = "ifassert"
        , fargs = ["x", "y"]
        , fpre = true
        , fpost = true
        , fmods = []
        , fbody = Seq
          [ Assign "lock" (Const 0)
          , If (Pred $ Var "x" :>=: Const 1)
               (Seq 
                 [ Assign "lock" (Const 1)
                 , Assert $ And [Pred $ Var "lock" :>=: Const 1, Pred $ Var "x" :>=: Const 1]
                 ])
               skip
          , If (Pred $ Var "y" :>=: Const 1)
               (Seq
                 [ Assign "lock" (Const 0)
                 , Assert $ And [Pred $ Var "lock" :<=: Const 0, Pred $ Var "y" :>=: Const 1]
                 ])
               skip
          ]
        }
      ]

    check "programs/neg/sum3.js"
      [ Function
        { fname = "sum"
        , fargs = ["n"]
        , fpre = true
        , fpost = true
        , fmods = []
        , fbody = Seq
          [ Assign "i" (Const 1)
          , Assign "sum" (Const 0)
          , While (Pred $ Var "i" :>=: Const 0)
                  (Pred $ Var "n" :>=: Var "i")
                  (Seq
                    [ Assign "j" (Const 1)
                    , While (Neg . Pred $ Var "i" :>=: Const 0)
                            (Pred $ Var "i" :>=: Var "j")
                            (Seq
                              [ Assign "sum" (add (Var "sum") (Var "j"))
                              , Assign "j" (add (Var "j") (Const 1))
                              , Assign "i" (add (Var "i") (Const 1))
                              ])
                    , Assert . Pred $ Var "sum" :>=: Const 0
                    ])
          ]
        }
      ]

    check "programs/neg/abs.js"
      [ Function
        { fname = "abs"
        , fargs = ["x"]
        , fpre = true
        , fpost = true
        , fmods = []
        , fbody = Seq
          [ Assign "res" (Const 0)
          , If (Pred $ Var "x" :>=: Const 1)
               (Assign "res" (Var "x"))
               (Assign "res" (sub (Const 0) (Var "x")))
          , Assert . Pred $ Var "res" :>=: Const 1
          ]
        }
      ]

    check "programs/neg/while5-false.js"
      [ Function
        { fname = "foo"
        , fargs = []
        , fpre = true
        , fpost = true
        , fmods = []
        , fbody = Seq
          [ Assign "x" (Const 0)
          , While (Pred $ Var "x" :<=: Const 6)
                  (Pred $ Var "x" :<=: Const 5)
                  (Assign "x" (add (Var "x") (Const 1)))
          , Assert . Pred $ Var "x" :==: Const 5
          ]
        }
      ]

    check "programs/neg/while5-noninductive.js"
      [ Function
        { fname = "foo"
        , fargs = []
        , fpre = true
        , fpost = true
        , fmods = []
        , fbody = Seq
          [ Assign "x" (Const 0)
          , While (Pred $ Var "x" :<=: Const 5)
                  (Pred $ Var "x" :<=: Const 5)
                  (Assign "x" (add (Var "x") (Const 1)))
          ]
        }
      ]

    check "programs/neg/fun3.js"
      [ Function
        { fname = "f"
        , fargs = ["x"]
        , fpre = Pred $ Var "x" :>=: Const 0
        , fpost = Pred $ Var "$result" :>=: Const 2
        , fmods = []
        , fbody = Seq
          [ Assign "x" $ BinOp Add (Var "x") (Const 2)
          , Return $ Var "x"
          ]
        }
      , Function
        { fname = "c"
        , fargs = ["x"]
        , fpre = true
        , fpost = Pred $ Var "$result" :>=: Const 2
        , fmods = []
        , fbody = Seq
          [ AppAsn "y" "f" [Var "x"]
          , Return $ Var "y"
          ]
        }
      ]

    check "programs/pos/modifies1.js"
      [ Function
        { fname = "f"
        , fargs = []
        , fpre = true
        , fpost = true
        , fmods = []
        , fbody = Seq
          [ ArrAsn "a" (Const 0) (Const 1)
          , AppAsn "x" "g" [Var "a"]
          , Assert . Pred $ Select (Array "a") (Const 1) :==: Const 2
          ]
        }
      , Function
        { fname = "g"
        , fargs = ["a"]
        , fpre = true
        , fpost = Pred $ Select (Array "a") (Const 1) :==: Const 2
        , fmods = ["a"]
        , fbody = Seq
          [ ArrAsn "a" (Const 1) (Const 2)
          , Return $ Const 0
          ]
        }
      ]

    check "programs/neg/modifies.js"
      [ Function
        { fname = "f"
        , fargs = []
        , fpre = true
        , fpost = true
        , fmods = []
        , fbody = Seq
          [ ArrAsn "a" (Const 0) (Const 1)
          , AppAsn "x" "g" [Var "a"]
          , Assert . Pred $ Select (Array "a") (Const 0) :==: Const 1
          ]
        }
      , Function
        { fname = "g"
        , fargs = ["a"]
        , fpre = true
        , fpost = true
        , fmods = ["a"]
        , fbody = Return $ Const 0
        }
      ]

    check "programs/pos/fun7.js"
      [ Function
        { fname = "f"
        , fargs = ["x", "y"]
        , fpre = true
        , fpost = Neg . Pred $ Var "$result" :>=: Const 0
        , fmods = []
        , fbody = Seq
          [ If (Neg . Pred $ Var "y" :<=: Const 0)
               (Return $ sub (Const 0) (Const 7))
               (Seq [])
          , Return $ sub (Const 0) (Const 1)
          ]
        }
      ]
