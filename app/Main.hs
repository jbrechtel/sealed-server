{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Sealed.User

main :: IO ()
main =
  scotty 3000 $ do
    get "/sealed" sealedOK
    get "/sealed/users" sealedUsers


sealedOK :: ActionM ()
sealedOK = text "OK!"

sealedUsers :: ActionM ()
sealedUsers = do
  json ([] :: [User])
