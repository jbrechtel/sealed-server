{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

main :: IO ()
main =
  scotty 3000 $
    get "/" $ do
      html "<h1>Scotty</h1>"
