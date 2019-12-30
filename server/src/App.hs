{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Concurrent
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger   (runStderrLoggingT)
import           Control.Monad.Trans.Except
import           Data.Map hiding (insert)
import qualified Data.Map as M
import           Database.Persist        hiding ( delete )
import           Database.Persist.Sqlite ( ConnectionPool, createSqlitePool
                                         , runSqlPool, runSqlPersistMPool
                                         , runMigration, selectList, (==.)
                                         , insert
                                         , entityVal)
import           Data.String.Conversions (cs)
import           Network.Wai
import           Network.Wai.MakeAssets
import           Servant

import           Api
import           Models

type WithAssets = Api :<|> Raw

withAssets :: Proxy WithAssets
withAssets = Proxy

options :: Options
options = Options "client"

app :: FilePath -> IO Application
app sqliteFile = serve withAssets <$> server sqliteFile

server :: FilePath -> IO (Server WithAssets)
server sqliteFile = do
  assets <- serveAssets options
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration migrateAll) pool
  db <- mkDB
  _ <- liftIO $ flip runSqlPersistMPool pool $ insert (Project "name")
  return (apiServer pool db :<|> Tagged assets)

apiServer :: ConnectionPool -> DB -> Server Api
apiServer pool db = listProjects pool
  :<|> listItem db
  :<|> getItem db
  :<|> postItem db
  :<|> deleteItem db

listProjects :: ConnectionPool -> Handler [Project]
listProjects pool = liftIO $ flip runSqlPersistMPool pool $ do
  maybeProjects <- selectList [] []
  let projects = Prelude.map (\entity@(Entity _ project) -> project) maybeProjects
  return projects

listItem :: DB -> Handler [ItemId]
listItem db = liftIO $ allItemIds db

getItem :: DB -> ItemId -> Handler Item
getItem db n = maybe (throwError err404) return =<< liftIO (lookupItem db n)

postItem :: DB -> String -> Handler ItemId
postItem db new = liftIO $ insertItem db new

-- fake DB

newtype DB = DB (MVar (Map ItemId String))

debug :: DB -> IO ()
debug (DB mvar) = readMVar mvar >>= print

mkDB :: IO DB
mkDB = DB <$> newMVar empty

insertItem :: DB -> String -> IO ItemId
insertItem (DB mvar) new = modifyMVar mvar $ \m -> do
  let newKey = case keys m of
        [] -> ItemId 0
        ks -> succ (maximum ks)
  return (M.insert newKey new m, newKey)

lookupItem :: DB -> ItemId -> IO (Maybe Item)
lookupItem (DB mvar) i = fmap (Item i) . Data.Map.lookup i <$> readMVar mvar

allItemIds :: DB -> IO [ItemId]
allItemIds (DB mvar) = keys <$> readMVar mvar

deleteItem :: MonadIO m => DB -> ItemId -> m ()
deleteItem (DB mvar) i = liftIO $ do
  modifyMVar_ mvar $ \m -> return (delete i m)
  return ()

