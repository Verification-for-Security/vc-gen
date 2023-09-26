module Config
  ( Config (..)
  , readConfig
  ) where

import System.Console.CmdArgs
import Control.Monad.IO.Class

data Config = Config
  { files :: [FilePath]
  -- ^ source files to check
  } deriving (Data, Typeable, Show, Eq)

config :: Config
config = Config
  { files = def -- &= typ "TARGET"
                -- &= args
                -- &= typFile
  } &= verbosity
    &= program "horn" 
    &= help    "The horn Verification System" 
    &= summary "horn" 
    &= details [ "horn is suite of toy program verifiers"
               , ""
               , "To check a file foo.js, type:"
               , "  horn -f foo.js"
               ]

readConfig :: MonadIO m => m Config 
readConfig = liftIO $ cmdArgs config 
