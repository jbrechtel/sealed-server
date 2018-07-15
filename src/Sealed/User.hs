{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Sealed.User
  ( User(..)
  , UserId
  , userIdToString
  ) where

import Web.Scotty (Parsable(..))
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.UUID (UUID, fromText, toString)

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

userIdToString :: UserId -> String
userIdToString (UserId uuid) = toString uuid

instance Parsable UserId where
  parseParam txt =
    case fromText $ toStrict txt of
      Just uuid -> Right $ UserId uuid
      Nothing -> Left "not a UUID"

newtype UserDisplayName = UserDisplayName Text
  deriving (ToJSON)

instance Parsable UserDisplayName where
  parseParam = Right . UserDisplayName . toStrict

newtype UserPublicKey = UserPublicKey Text
  deriving (ToJSON)

instance Parsable UserPublicKey where
  parseParam = Right . UserPublicKey . toStrict
