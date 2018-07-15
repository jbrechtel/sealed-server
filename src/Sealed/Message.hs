{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sealed.Message
  ( Message(..)
  ) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)

data Message =
  Message { messageId :: MessageId
          , messageName :: MessageName
          , messageBody :: MessageBody
          }

newtype MessageId = MessageId UUID
  deriving (ToJSON)

newtype MessageName = MessageName Text
  deriving (ToJSON)

newtype MessageBody = MessageBody Text
  deriving (ToJSON)
