module Sealed.Config
  ( Config(..)
  , loadConfigFromEnvironment
  ) where

import System.Environment (getEnv)

data Config =
  Config { configDataPath :: String }

loadConfigFromEnvironment :: IO Config
loadConfigFromEnvironment = do
  dataPath <- getEnv "SEALED_DATA_PATH"
  pure $ Config dataPath
