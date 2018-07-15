module Sealed.Storage
  ( storeUser
  , loadUsers
  , storeMessage
  , loadMessages
  ) where

import Sealed.Config
import Sealed.Message
import Sealed.User (User(..), UserId, userIdToString)

import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))
import Data.Aeson (encodeFile)

storeMessage :: Config -> UserId -> Message -> IO ()
storeMessage _ _ _ = pure ()

storeUser :: Config -> User -> IO ()
storeUser config user = do
  createDirectoryIfMissing True $ messagesPath config user
  encodeFile (userPath config user) user

loadMessages :: Config -> UserId -> IO [Message]
loadMessages _ _ = pure []

loadUsers :: Config -> IO [User]
loadUsers _ = pure []

userPath :: Config -> User -> FilePath
userPath config user =
  (configDataPath config) </> (userIdToString $ userId user) <> ".json"

messagesPath :: Config -> User -> FilePath
messagesPath config user =
  (configDataPath config) </> (userIdToString $ userId user) </> "messages"
