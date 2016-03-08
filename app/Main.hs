{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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

import qualified Database.Redis as R
import Web.Scotty.Trans
import TConfig
import Token
import Options.Applicative as O
import Control.Lens
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors
import qualified Data.Text.Lazy as LT
import Data.Configurator
import Control.Monad.Trans
import Network.HTTP.Types.Status
    
data TOptions = TOptions { _port :: Int, _config :: String } deriving Show

makeLenses ''TOptions
             
optionParser :: Parser TOptions
optionParser =
    TOptions
    <$> option auto
        ( long "port"   <> short 'p' <> metavar "PORT"   <> help "port to start the service" )
    <*> strOption
        ( long "config" <> short 'c' <> metavar "CONFIG" <> help "config file for twergo")

tokenOptions = execParser optParser
    where optParser = O.info (helper <*> optionParser)
                      ( fullDesc
                        <>  progDesc "Start token service on PORT"
                        <>  O.header "Token service for Traderwave" )

-- | 
tokenService :: R.Connection -> ScottyT LT.Text IO ()
tokenService conn = do
  middleware logStdout
  middleware simpleCors
  let retrieve = "/retrieve"
  options retrieve $ status status200
  get retrieve $ do
    token  <- jsonData 
    result <- liftIO $ retrieveData conn token
    case result of
      Nothing -> do
                  status notFound404
                  json $ UserError "Invalid token or expired token"
      Just r  -> json $ UserData r
  let register = "/register"
  options register $ status status200
  post register $ do
    (RegistryData e (UserData v)) <- jsonData
    token <- liftIO $ registerData conn v (toInteger e)
    json token
  
main :: IO ()
main = do
  opts  <- tokenOptions
  putStrLn $ "Loading configuration " ++ (opts ^. config)
  cfg   <- load [ Required $ opts ^. config ]
  cinfo <- getConnectInfo cfg
  conn  <- R.connect cinfo
  scottyT (opts ^. port) id $ tokenService conn
