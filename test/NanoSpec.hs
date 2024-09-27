module NanoSpec
  ( rubric
  ) where

import System.Directory (listDirectory)
import Data.IORef

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import Data.Maybe (fromJust)
import Control.Monad.Trans (lift)
import Control.Monad (when)
import Control.Exception (catch, SomeException)

import Util
import qualified Parse
import qualified Nano

verify :: String -> IO Bool
verify file = do
  prog <- Parse.nano file
  Nano.check $ fromJust prog

checkWithMirror :: IORef Bool -> FilePath -> FilePath -> Spec
checkWithMirror correctVerifier file mirror = do
  let parse f = runIO $ normalize . fromJust <$> Parse.nano f
  prog <- parse file
  prog' <- parse mirror

  it "you provided invariants that satisfy the constraints" $ do
    valid <- Nano.check prog
    valid @?= True

  it "did not change anything besides invariants" $ do
    stripInv prog @?= prog'

  it "the verifier is correct" $ do
    isCorrect <- readIORef correctVerifier
    isCorrect @?= True

getPrograms :: FilePath -> RubricM () [FilePath]
getPrograms = lift . runIO . listDirectory

rubric :: Rubric
rubric = do
  -- This tracks whether the verifier is completely correct. Tests that rely
  -- on a correct verifier can fail by checking this value. Note that the
  -- ordering of the tests is important here!
  correctVerifier <- lift . runIO . newIORef $ True

  criterion "instance VCGen Statement" 0.6 $ do
    bcriterion "vcgen" 1.0 0.5 $ do
      let getPrograms' dir = fmap (dir <>) <$> getPrograms dir
      let check b f = dpasses f $ do
            valid <- verify f `catch` \(_ :: SomeException) -> do
              return $ not b
            when (valid /= b) $ do
              writeIORef correctVerifier False
            valid @?= b

      criterion "rejects negative files" 0.5 . distribute $ do
        let dir = "programs/neg/"
        neg <- getPrograms' dir

        mapM_ (check False) neg

      criterion "accepts positive files" 0.5 . distribute $ do
        let dir = "programs/pos/"
        pos <- getPrograms' dir

        mapM_ (check True) pos

  criterion "verification by adding invariants" 0.4 . distribute $ do
    let invdir = "programs/verify/"
    let mirdir = "programs/.verify/"

    files <- getPrograms mirdir
    let inv = (invdir <>) <$> files
    let mirror = (mirdir <>) <$> files

    let full = zip inv mirror

    let check (f, m) = dcriterion f . passOrFail $ checkWithMirror correctVerifier f m
    mapM_ check full
