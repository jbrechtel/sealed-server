{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Sealed.Message
  ( Message(..)
  , MessageId
  , nextMessageId
  , mkMessageBody
  , messageIdToString
  ) where

import Web.Scotty (Parsable(..))
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, (.=), (.:))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.UUID (UUID, toString)
import Data.UUID.V4 (nextRandom)

data Message =
  Message { messageId :: MessageId
          , messageName :: MessageName
          , messageBody :: MessageBody
          }

instance ToJSON Message where
  toJSON msg =
    object [ "message_id"   .= messageId msg
           , "message_name" .= messageName msg
           , "message_body" .= messageBody msg
           ]

instance FromJSON Message where
  parseJSON (Object o) =
    Message <$> o .: "message_id"
            <*> o .: "message_name"
            <*> o .: "message_body"

  parseJSON _ = fail "Message JSON invalid"


nextMessageId :: IO MessageId
nextMessageId = MessageId <$> nextRandom

mkMessageBody :: ByteString -> MessageBody
mkMessageBody = MessageBody . decodeUtf8

messageIdToString :: MessageId -> String
messageIdToString (MessageId msgId) = toString msgId

newtype MessageId = MessageId UUID
  deriving (ToJSON, FromJSON)

newtype MessageName = MessageName Text
  deriving (ToJSON, FromJSON)

instance Parsable MessageName where
  parseParam = Right . MessageName . toStrict

newtype MessageBody = MessageBody Text
  deriving (ToJSON, FromJSON)
