module Sealed.Storage
  ( storeUser
  , loadUsers
  , loadUser
  , storeMessage
  , loadMessages
  , clearMessages
  , userExists
  ) where

import Sealed.Config
import Sealed.Message (Message(..), messageIdToString)
import Sealed.User (User(..), UserId, userIdToString)

import System.Directory (createDirectoryIfMissing, listDirectory, removeFile, doesFileExist)
import System.FilePath.Posix ((</>))
import Data.Aeson (encodeFile, decodeFileStrict)
import Data.Maybe (catMaybes)

storeMessage :: Config -> UserId -> Message -> IO ()
storeMessage config uId message = do
  createDirectoryIfMissing True $ messagesPath config uId
  encodeFile (messagePath config uId message) message

storeUser :: Config -> User -> IO ()
storeUser config user = do
  createDirectoryIfMissing True $ messagesPath config (userId user)
  createDirectoryIfMissing True $ usersPath config
  encodeFile (userPath config $ userId user) user

clearMessages :: Config -> UserId -> IO ()
clearMessages config uId = do
  msgFiles <- messageFiles config uId
  mapM_ removeFile msgFiles

loadMessages :: Config -> UserId -> IO [Message]
loadMessages config uId = do
  msgFiles <- messageFiles config uId
  messages <- mapM decodeFileStrict msgFiles
  pure $ catMaybes messages

messageFiles :: Config -> UserId -> IO [FilePath]
messageFiles config uId = do
  relativeMessageFiles <- listDirectory $ messagesPath config uId
  pure $ ((messagesPath config uId) </>) <$> relativeMessageFiles

loadUsers :: Config -> IO [User]
loadUsers config = do
  relativeUserFiles <- listDirectory $ usersPath config
  let absoluteUserFiles = ((usersPath config) </>) <$> relativeUserFiles
  users <- mapM decodeFileStrict absoluteUserFiles
  pure $ catMaybes users

loadUser :: Config -> UserId -> IO (Maybe User)
loadUser config uId = do
  decodeFileStrict $ userPath config uId

userExists :: Config -> UserId -> IO Bool
userExists config uId = doesFileExist $ userPath config uId

usersPath :: Config -> FilePath
usersPath config =
  (configDataPath config) </> "users"

userPath :: Config -> UserId -> FilePath
userPath config uId =
  (usersPath config) </> (userIdToString $ uId) <> ".json"

messagesPath :: Config -> UserId -> FilePath
messagesPath config uId =
  (configDataPath config) </> (userIdToString $ uId) </> "messages"

messagePath :: Config -> UserId -> Message -> FilePath
messagePath config uId message =
  (messagesPath config uId) </> (messageIdToString $ messageId message) <> ".json"
