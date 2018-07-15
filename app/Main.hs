{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Sealed.Config
import Sealed.User
import Sealed.Storage
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main =
  scotty 3000 $ do
    get "/sealed" sealedOK
    get "/sealed/users" listUsers
    post "/sealed/users/:user_id" createUser


sealedOK :: ActionM ()
sealedOK = text "OK!"

listUsers :: ActionM ()
listUsers = do
  json ([] :: [User])

createUser :: ActionM ()
createUser = do
  config <- liftIO loadConfigFromEnvironment
  user <- User <$> param "user_id"
               <*> param "display_name"
               <*> param "public_key"
  liftIO $ storeUser config user
  json ("OK!" :: String)
