{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Token Server 
Copyright   : (c) Traderwave Ltd, 2016
License     : All Rights Reserved
Maintainer  : yogesh@traderwave.com
Stability   : experimental
Portability : POSIX, WINDOWS

Token server can be used to temporarily store JSON data. It works by allowing 
user to store arbitrary JSON data and returning a token for it. User can specify
the expiry period, or a default period of 24hrs is assumed. 

After expiry the token, and hence the data expires.
-}
module Main where

import Web.Scotty.Trans
import qualified Web.Scotty as S
import Token
import Data.Configurator
import Data.Configurator.Types (Config)
import Database.Redis

data TokenConfig 

getConnectInfo :: Config -> IO ConnectInfo
getConnectInfo cfg = do
  mode <- require cfg "token-server.mode" :: IO String
  case mode of
    "bluemix" -> do
              service <- require cfg "token-server.name" :: IO String
              loadBluemixVCap service
    "config"  -> loadLocalInfo cfg
    _         -> error "Invalid token-server mode"
    

loadLocalInfo :: Config -> IO ConnectInfo
loadLocalInfo cfg = do
  host <- require cfg "token-server.host"
  port <- require cfg "token-server.port" :: IO Int
  return $ defaultConnectInfo { connectHost = host, connectPort = fromIntegral port }

loadBluemixVCap :: String -> IO ConnectInfo
loadBluemixVCap service = do
  undefined
  
main :: IO ()
main = undefined
