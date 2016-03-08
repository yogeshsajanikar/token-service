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

-- | Token is a random ascii string (typically of length 6)
newtype Token = Token B.ByteString deriving Show

instance ToJSON Token where
  toJSON (Token t) = object [ "token" .= TE.decodeUtf8 t ]

instance FromJSON Token where
  parseJSON (Object v) = Token . TE.encodeUtf8  <$> v .: "token"
  parseJSON _ = undefined

newtype UserData = UserData { userData :: T.Text }

instance ToJSON UserData where
  toJSON (UserData d) = object [ "data" .= d ]

instance FromJSON UserData where
  parseJSON (Object v) = UserData <$> v .: "data"
  parseJSON _ = undefined

data TokenData = TokenData { token :: Token
                           , userdata :: UserData }




-- | Map of ASCII characteres allowed in token
-- We consider range [a-z,A-Z,0-9]. This map is used for creating a token
base62 :: M.Map Int Char
base62 = M.fromList zchrs
  where
    chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    zchrs = zip [0..] chars

-- | Generate a token of given length
-- Creates a token of given length, randomly generating the ascii values
-- with the help of given random generator.
genToken :: (RandomGen g) => g -> Int -> String
genToken g n = map (\k -> fromJust $ M.lookup k base62) $ take n $ randomRs (mn,mx) g
  where
    mn = fst $ M.findMin base62
    mx = fst $ M.findMax base62

createToken :: Int -> IO B.ByteString
createToken n = B8.pack . flip genToken n <$> newStdGen

-- | Register the data, and get a token
registerData :: R.Connection -> Value -> Integer -> IO Token
registerData c d t = do
  token <- createToken 6
  R.runRedis c $ do
    R.set token (LB.toStrict $ encode d)
    R.expire token t
  return $ Token token

-- | Retrieve the data, by supplying a token
retrieveData :: R.Connection -> Token -> IO (Maybe Value)
retrieveData c (Token t) = do
    val <- R.runRedis c $ R.get t
    return $ join $ (decode . LB.fromStrict) <$> join (val ^? _Right) 
