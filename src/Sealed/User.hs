{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Sealed.User
  ( User(..)
  , UserId
  , UserApiKey(..)
  , userIdToString
  ) where

import Web.Scotty (Parsable(..))
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, (.=), (.:))
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.UUID (UUID, fromText, toString)

data User =
  User { userId :: UserId
       , userDisplayName :: UserDisplayName
       , userPublicKey :: UserPublicKey
       , userApiKey :: UserApiKey
       }

instance ToJSON User where
  toJSON user =
    object [ "display_name" .= userDisplayName user
           , "public_key"   .= userPublicKey user
           , "user_id"      .= userId user
           ]

instance FromJSON User where
  parseJSON (Object o) =
    User <$> o .: "user_id"
         <*> o .: "display_name"
         <*> o .: "public_key"
         <*> o .: "api_key"

  parseJSON _ = fail "User JSON invalid"

newtype UserId = UserId UUID
  deriving (ToJSON, FromJSON)

userIdToString :: UserId -> String
userIdToString (UserId uuid) = toString uuid

instance Parsable UserId where
  parseParam txt =
    case fromText $ toStrict txt of
      Just uuid -> Right $ UserId uuid
      Nothing -> Left "not a UUID"

newtype UserDisplayName = UserDisplayName Text
  deriving (ToJSON, FromJSON)

instance Parsable UserDisplayName where
  parseParam = Right . UserDisplayName . toStrict

newtype UserPublicKey = UserPublicKey Text
  deriving (ToJSON, FromJSON)

instance Parsable UserPublicKey where
  parseParam = Right . UserPublicKey . toStrict

newtype UserApiKey = UserApiKey Text
  deriving (Eq, ToJSON, FromJSON)

instance Parsable UserApiKey where
  parseParam = Right . UserApiKey . toStrict
