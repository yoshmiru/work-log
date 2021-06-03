import           Data.Yaml.Config
import           Network.Wai.Handler.Warp
import           System.IO

import           App
import           ConfigParser

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
