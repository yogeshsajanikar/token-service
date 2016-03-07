{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Token
Description : Token service for JSON data
Copyright   : (c) Traderwave Ltd, 2015
License     : All Rights Reserved
Maintainer  : Yogesh Sajanikar <yogesh@traderwave.com>
Stability   : experimental
Portability : POSIX, WINDOWS

Represents expirable token service 
-}
module Token
    (
     Token(..),
     registerData,
     retrieveData
    ) where

import Data.Maybe
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import Data.Locator
import qualified Database.Redis as R
import Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import Control.Lens

newtype Token = Token B.ByteString

registerData :: R.Connection -> Value -> Integer -> IO Token
registerData c d t = do
  uuid <- U.toASCIIBytes <$> U.nextRandom 
  let token = hashStringToBase62 12 $ uuid
  R.runRedis c $ do
    R.set token (toStrict $ encode d)
    R.expire token t
  return $ Token token

retrieveData :: R.Connection -> Token -> IO (Maybe Value)
retrieveData c (Token t) = do
    val <- R.runRedis c $ R.get t
    return $ join $ (decode . fromStrict) <$> join (val ^? _Right) 
