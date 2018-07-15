module Sealed.Storage
  ( storeUser
  , loadUsers
  , storeMessage
  , loadMessages
  ) where

import Sealed.Message
import Sealed.User

storeMessage :: UserId -> Message -> IO ()
storeMessage _ _ = pure ()

storeUser :: User -> IO ()
storeUser _ = pure ()

loadMessages :: UserId -> IO [Message]
loadMessages _ = pure []

loadUsers :: IO [User]
loadUsers = pure []

