{-# LANGUAGE OverloadedStrings #-}
module Token.Registry
(
 registerData,
 retrieveData
)
where

import Token.Data
import Data.Maybe
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Lens
import Control.Monad
import Control.Lens hiding ((.=))
import qualified Database.Redis as R
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Random

-- | Register the data, and get a token
registerData :: R.Connection -> Value -> Integer -> IO Token
registerData c d t = do
  token <- createToken 6
  let ts = tokenString token
  R.runRedis c $ do
    R.set ts (LB.toStrict $ encode d)
    R.expire ts t
  return token

-- | Retrieve the data, by supplying a token
retrieveData :: R.Connection -> Token -> IO (Maybe Value)
retrieveData c t = do
    val <- R.runRedis c $ R.get $ tokenString t
    return $ join $ (decode . LB.fromStrict) <$> join (val ^? _Right) 

