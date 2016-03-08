{-# LANGUAGE OverloadedStrings #-}
module TConfig where

import Data.Maybe    
import Data.Configurator
import Data.Configurator.Types (Config)
import Database.Redis
import Data.Aeson.Lens
import Data.Aeson
import Control.Lens
import System.Environment
import Data.Text as T
import Data.Text.Lens
import Data.Text.Encoding 

    
-- | Load the configuration file
getConnectInfo :: Config -> IO ConnectInfo
getConnectInfo cfg = do
  mode <- require cfg "token.server.mode" :: IO String
  case mode of
    "bluemix" -> do
              service <- require cfg "token.db.service" :: IO String
              label   <- require cfg "token.db.label" :: IO String
              loadBluemixVCap service label
    "config"  -> loadLocalInfo cfg
    _         -> error "Invalid token-server mode"
    

-- | Load local information
loadLocalInfo :: Config -> IO ConnectInfo
loadLocalInfo cfg = do
  host <- require cfg "token.server.host"
  port <- PortNumber . fromIntegral <$> (require cfg "token.server.port" :: IO Int)
  return $ defaultConnectInfo { connectHost = host, connectPort = port }

loadBluemixVCap :: String -> String -> IO ConnectInfo
loadBluemixVCap service _ = do
  vcap <- (\v -> _Text # v :: Text ) <$> getEnv "VCAP_SERVICES"
  let msec = vcap ^? key (T.pack service)
      cred = msec ^? _Just . nth 0 . key "credentials"
      host = T.unpack <$> cred ^? _Just . key "host" . _String
      port = cred ^? _Just . key "port" . _Integer
      pass = encodeUtf8 <$> cred ^? _Just . key "password" . _String
      cinfo = cn <$> host <*> port <*> pass
  return $ fromMaybe defaultConnectInfo cinfo
      where
        cn h p ps =
            defaultConnectInfo { connectHost = h
                               , connectPort = PortNumber . fromIntegral $ p
                               , connectAuth = Just ps }
