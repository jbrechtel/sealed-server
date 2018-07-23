{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Sealed.Config
import Sealed.Routes (routes)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  config <- liftIO loadConfigFromEnvironment
  scotty (configPort config) (routes config)

