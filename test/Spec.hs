import Test.Hspec
import Test.Hrubric
import Test.HUnit

import Control.Monad
import Control.Monad.Trans.Maybe
import Grade

import qualified ExprSpec
import qualified ParseSpec
import qualified NanoSpec

rubric :: Rubric
rubric = do
  criterion "It compiles" (1/10) . passOrFail $ 
    it "..." $ True @?= True
  criterion "Expr" (1/10) ExprSpec.rubric
  criterion "Parse" (3/10) ParseSpec.rubric
  criterion "Nano" (5/10) NanoSpec.rubric

main :: IO ()
main = void . runMaybeT $ do
  result <- MaybeT $ hrubric rubric
  pretty result
  autograde result
