module Sealed.Config
  ( Config(..)
  , loadConfigFromEnvironment
  ) where

import System.Environment (getEnv)

data Config =
  Config { configDataPath :: String
         , configPort :: Int
         }

loadConfigFromEnvironment :: IO Config
loadConfigFromEnvironment = do
  dataPath <- getEnv "SEALED_DATA_PATH"
  port <- read <$> getEnv "SEALED_SERVER_PORT"
  pure $ Config dataPath port
