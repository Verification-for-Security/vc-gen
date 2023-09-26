module NanoSpec
  ( rubric
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import System.IO.Unsafe
import Data.Maybe (fromJust)

import Util
import qualified Parse
import qualified Nano

verify :: String -> IO Bool
verify file = do
  prog <- Parse.nano file
  Nano.check $ fromJust prog

withInv :: (FilePath, FilePath) -> Spec
withInv (file, mirror) = do
  let parse f = runIO $ normalize . fromJust <$> Parse.nano f
  prog <- parse file
  prog' <- parse mirror

  it "did not change anything besides invariants" $ do
    stripInv prog @?= prog'

  it "you provided invariants that satisfy the constraints" $ do
    Nano.check prog `shouldReturn` True

positive :: Rubric
positive = do
  criterion "vcgen" 0.6 . distribute $ do
    let posdir = "programs/pos/"
    let pos = (posdir <>) <$> getPrograms posdir

    let check b f = dpasses f $ verify f >>= (@?= b)

    mapM_ (check True)  pos

  criterion "verification by adding invariants" 0.4 . distribute $ do
    let invdir = "programs/verify/"
    let mirdir = "programs/.verify/"

    let files = getPrograms mirdir
    let inv = (invdir <>) <$> files
    let mirror = (mirdir <>) <$> files

    let full = zip inv mirror

    let check (f, m) = dcriterion f . passOrFail $ do
          withInv (f, m)
    mapM_ check full

negative :: Float
negative = fromJust . unsafePerformIO . hrubric . distribute $ do
  let negdir = "programs/neg/"
  let neg = (negdir <>) <$> getPrograms negdir

  let check b f = dpasses f $ verify f >>= (@?= b)

  mapM_ (check False) neg

rubric :: Rubric
rubric
  | negative > 0.99 = positive
  | otherwise = passes "Too lenient on negative tests, not running positive" 1 $ do
    True @?= False
