{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Sealed.User
  ( User(..)
  , UserId
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import Data.UUID (UUID)

data User =
  User { userId :: UserId
       , userDisplayName :: UserDisplayName
       , userPublicKey :: UserPublicKey
       }

instance ToJSON User where
  toJSON user =
    object [ "display_name" .= userDisplayName user
           , "public_key"   .= userPublicKey user
           , "user_id"      .= userId user
           ]

newtype UserId = UserId UUID
  deriving (ToJSON)

newtype UserDisplayName = UserDisplayName Text
  deriving (ToJSON)

newtype UserPublicKey = UserPublicKey Text
  deriving (ToJSON)
