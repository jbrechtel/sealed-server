module Sealed.Storage
  ( storeUser
  , loadUsers
  , storeMessage
  , loadMessages
  ) where

import Sealed.Config
import Sealed.Message
import Sealed.User (User(..), UserId, userIdToString)

import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath.Posix ((</>))
import Data.Aeson (encodeFile, decodeFileStrict)
import Data.Maybe (catMaybes)

storeMessage :: Config -> UserId -> Message -> IO ()
storeMessage _ _ _ = pure ()

storeUser :: Config -> User -> IO ()
storeUser config user = do
  createDirectoryIfMissing True $ messagesPath config user
  createDirectoryIfMissing True $ usersPath config
  encodeFile (userPath config user) user

loadMessages :: Config -> UserId -> IO [Message]
loadMessages _ _ = pure []

loadUsers :: Config -> IO [User]
loadUsers config = do
  relativeUserFiles <- listDirectory $ usersPath config
  let absoluteUserFiles = ((usersPath config) </>) <$> relativeUserFiles
  users <- mapM decodeFileStrict absoluteUserFiles
  pure $ catMaybes users

usersPath :: Config -> FilePath
usersPath config =
  (configDataPath config) </> "users"

userPath :: Config -> User -> FilePath
userPath config user =
  (usersPath config) </> (userIdToString $ userId user) <> ".json"

messagesPath :: Config -> User -> FilePath
messagesPath config user =
  (configDataPath config) </> (userIdToString $ userId user) </> "messages"
