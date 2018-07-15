module Sealed.Storage
  ( store
  , load
  ) where

import Sealed.Message

store :: Message -> IO ()
store _ = pure ()

load :: IO [Message]
load = pure []

