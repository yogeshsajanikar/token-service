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
import TConfig
  
main :: IO ()
main = undefined
