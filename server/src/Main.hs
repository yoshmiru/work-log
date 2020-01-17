{-# LANGUAGE OverloadedStrings          #-}

import           Data.Aeson
import           Data.Yaml.Config
import           Network.Wai.Handler.Warp
import           System.IO

import           App

data HttpConfig = HttpConfig {
  httpPort :: Int
}

data DBConfig = DBConfig {
  host :: String,
  dbname :: String,
  user :: String,
  password :: String,
  dbPort :: Int
}

data Config = Config {
  http :: HttpConfig,
  db :: DBConfig
}

instance FromJSON HttpConfig where
  parseJSON = withObject "HttpConfig" $ \v ->
    HttpConfig <$> v .: "port"

instance FromJSON DBConfig where
  parseJSON = withObject "DBConfig" $ \v ->
    DBConfig <$> v .: "host"
           <*> v .: "dbname"
           <*> v .: "user"
           <*> v .: "password"
           <*> v .: "port"

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v ->
    Config <$> v .: "http"
           <*> v .: "db"

main :: IO ()
main = do
  config <- loadYamlSettings [ "config.yaml" ] [] useEnv :: IO Config
  let port     = 3000
      settings' (Config (HttpConfig httpPort) _) =
        setPort httpPort $ setBeforeMainLoop
          (hPutStrLn stderr ("listening on port " ++ show httpPort ++ "..."))
          defaultSettings
      app' (Config _ (DBConfig host dbname user password dbPort)) =
        app $ "host=" ++ host ++ " dbname=" ++ dbname ++ " user=" ++ user ++
             " password=" ++ password ++ " port=" ++ (show dbPort)
  runSettings (settings' config) =<< app' config
