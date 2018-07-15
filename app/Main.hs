{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.ByteString.Lazy (toStrict)
import Sealed.Config
import Sealed.User
import Sealed.Message
import Sealed.Storage
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  config <- liftIO loadConfigFromEnvironment
  scotty 3000 $ do
    get "/sealed" sealedOK
    get "/sealed/users" $ listUsers config
    post "/sealed/users/:user_id" $ createUser config
    post "/sealed/users/:user_id/messages/:name" $ postMessage config
    get "/sealed/users/:user_id/messages" $ listMessages config


sealedOK :: ActionM ()
sealedOK = text "OK!"

listUsers :: Config -> ActionM ()
listUsers config = do
  users <- liftIO $ loadUsers config
  json users

listMessages :: Config -> ActionM ()
listMessages config = do
  uId <- param "user_id"
  messages <- liftIO $ loadMessages config uId
  liftIO $ clearMessages config uId
  json messages

createUser :: Config -> ActionM ()
createUser config = do
  user <- User <$> param "user_id"
               <*> param "display_name"
               <*> param "public_key"
  liftIO $ storeUser config user
  json ("OK!" :: String)

postMessage :: Config -> ActionM ()
postMessage config = do
  uId <- param "user_id"
  msgId <- liftIO nextMessageId
  msgName <- param "name"
  msgBody <- (mkMessageBody . toStrict) <$> body
  let message = Message msgId msgName msgBody
  liftIO $ storeMessage config uId message
  json ("OK!" :: String)
