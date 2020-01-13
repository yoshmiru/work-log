
import           Network.Wai.Handler.Warp
import           System.IO

import           App

main :: IO ()
main = do
  let port     = 3000
      settings = setPort port $ setBeforeMainLoop
        (hPutStrLn stderr ("listening on port " ++ show port ++ "..."))
        defaultSettings
  runSettings settings =<< app "host=arjuna.db.elephantsql.com dbname=eommxfoh user=eommxfoh password=FA3ExM-xneFuQIB7aY1xoPDq2EfDt0U8 port=5432"
