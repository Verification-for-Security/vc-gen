module Util 
  ( Normalize (..)
  , stripInv
  ) where

import Prelude hiding (and, or, seq)
import Logic
import Nano

class Normalize a where
  normalize :: a -> a

instance Normalize (Logic a) where
  normalize (Neg l) = neg $ normalize l
  normalize (And [l]) = normalize l
  normalize (And ls) = and $ normalize <$> ls
  normalize (Pred p) = Pred p
  normalize (Forall x l) = Forall x $ normalize l

instance Normalize (Statement a) where
  normalize (Seq [s]) = normalize s
  normalize (Seq ss) = seq $ normalize <$> ss
  normalize (If cond body0 body1) = If (normalize cond) (normalize body0) (normalize body1)
  normalize (While inv cond body) = While (normalize inv) (normalize cond) (normalize body)
  normalize (Assume l) = Assume $ normalize l
  normalize (Assert l) = Assert $ normalize l
  normalize e = e

instance Normalize (Nano a) where
  normalize = (normalize <$>) 

instance Normalize (Function a) where
  normalize func = Function
    { fname = fname func
    , fargs = fargs func
    , fbody = normalize $ fbody func
    , fpre = normalize $ fpre func
    , fpost = normalize $ fpost func
    , fmods = fmods func
    }

stripInv :: Nano a -> Nano a
stripInv = (stripFunc <$>)
  where
    stripFunc func = func { fbody = stripInv' $ fbody func }

stripInv' :: Statement a -> Statement a
stripInv' (Seq ss) = seq $ stripInv' <$> ss
stripInv' (If cond body0 body1) = If cond (stripInv' body0) (stripInv' body1)
stripInv' (While _ cond body) = While true cond (stripInv' body)
stripInv' e = e
