module AppSpec where

import           Control.Exception                        ( throwIO
                                                          , ErrorCall(..)
                                                          )
import           Control.Monad.Trans.Except
import           Data.Yaml.Config
import           Network.HTTP.Client                      ( Manager
                                                          , newManager
                                                          , defaultManagerSettings
                                                          )
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp
import           Servant.API
import           Servant.Client
import           Test.Hspec

import           Api
import           App                                      ( app )
import           ConfigParser

--getItemIds :: ClientM [ItemId]
--getItem :: ItemId -> ClientM Item
--postItem :: String -> ClientM ItemId
--deleteItem :: ItemId -> ClientM ()
--getItemIds :<|> getItem :<|> postItem :<|> deleteItem = client api
--
spec :: Spec
spec = do
  describe "app" $ do -- $ around withApp $ do
    it "not implemented yet" $ \_ -> putStrLn "not implemented yet"
--    context "/api/item" $ do
--      it "returns an empty list" $ \host -> do
--        try host getItemIds `shouldReturn` []
--
--      context "/api/item/:id" $ do
--        it "returns a 404 for missing items" $ \(manager, baseUrl) -> do
--          Left err <- runClientM (getItem $ ItemId 23)
--                                 (ClientEnv manager baseUrl Nothing)
--          errorStatus err `shouldBe` (Just notFound404)
--
--      context "POST" $ do
--        it "allows to create an item" $ \host -> do
--          i <- try host $ postItem "foo"
--          try host (getItem i) `shouldReturn` Item i "foo"
--
--        it "lists created items" $ \host -> do
--          i <- try host $ postItem "foo"
--          try host getItemIds `shouldReturn` [i]
--
--        it "lists 2 created items" $ \host -> do
--          a <- try host $ postItem "foo"
--          b <- try host $ postItem "bar"
--          try host getItemIds `shouldReturn` [a, b]
--
--      context "DELETE" $ do
--        it "allows to delete items" $ \host -> do
--          i <- try host $ postItem "foo"
--          _ <- try host $ deleteItem i
--          try host getItemIds `shouldReturn` []
--
type Host = (Manager, BaseUrl)
--
--try :: Host -> ClientM a -> IO a
--try (manager, baseUrl) action = do
--  result <- runClientM action (ClientEnv manager baseUrl Nothing)
--  case result of
--    Right x   -> return x
--    Left  err -> throwIO $ ErrorCall $ show err
--
withApp :: (Host -> IO a) -> IO a
withApp action = do
  config <- loadYamlSettings [ "config.yaml" ] [] useEnv :: IO Config
  let app' (Config _ (DBConfig host dbname user password dbPort)) =
        app $ "host=" ++ host ++ " dbname=" ++ dbname ++ " user=" ++ user ++
             " password=" ++ password ++ " port=" ++ (show dbPort)
  testWithApplication (app' config) $ \port -> do
    manager <- newManager defaultManagerSettings
    let url = BaseUrl Http "localhost" port ""
    action (manager, url)
--
--errorStatus :: ServantError -> Maybe Status
--errorStatus servantError = case servantError of
--  FailureResponse response -> Just $ responseStatusCode response
--  _                        -> Nothing
--
