{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Token.Data
(
 Token(),
 tokenString,
 createToken,
 UserData(..),
 UserError(..),
 RegistryData(..)
)               
where

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

-- | Get the token string from the token
tokenString :: Token -> B.ByteString
tokenString (Token s) = s

instance ToJSON Token where
  toJSON (Token t) = object [ "token" .= TE.decodeUtf8 t ]

instance FromJSON Token where
  parseJSON (Object v) = Token . TE.encodeUtf8  <$> v .: "token"
  parseJSON _ = undefined

newtype UserData = UserData Value

instance ToJSON UserData where
  toJSON (UserData d) = object [ "data" .= d ]

instance FromJSON UserData where
  parseJSON (Object v) = UserData <$> v .: "data"
  parseJSON _ = undefined

-- | Token data 
data RegistryData = RegistryData { expiresIn :: Int
                                 , userData  :: UserData }

$(deriveJSON defaultOptions ''RegistryData)

-- | User error
data UserError = UserError { error :: String }

$(deriveJSON defaultOptions ''UserError)

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

createToken :: Int -> IO Token
createToken n = Token . B8.pack . flip genToken n <$> newStdGen

