{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module App where

import           Control.Concurrent
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger   (runStderrLoggingT)
import           Control.Monad.Trans.Except
import           Data.Map hiding (delete, insert, map)
import qualified Data.Map as M
import           Database.Persist
import           Database.Persist.Postgresql ( ConnectionPool, createPostgresqlPool
--import           Database.Persist.Sqlite ( ConnectionPool, createSqlitePool
                                         , runSqlPool, runSqlPersistMPool
                                         , runMigration, selectList, (==.)
                                         , insert , entityVal
                                         , fromSqlKey, toSqlKey)
import           Data.String.Conversions (cs)
import qualified Data.Text as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           GHC.Int
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

server :: String -> IO (Server WithAssets)
server connStr = do
  assets <- serveAssets options
  pool <- runStderrLoggingT $ do
    createPostgresqlPool (cs connStr) 5

  runSqlPool (runMigration migrateAll) pool
  db <- mkDB
  --_ <- liftIO $ flip runSqlPersistMPool pool $ insert (Project "name")
  return (apiServer pool db :<|> Tagged assets)

apiServer :: ConnectionPool -> DB -> Server Api
apiServer pool db =
      (listProjects pool :<|> postProject pool)
  :<|> (listWorks pool :<|> getWorks pool :<|> deleteWork pool
    :<|> postWork pool)
  :<|> (listItem db
    :<|> getItem db
    :<|> postItem db
    :<|> deleteItem db)

listProjects :: ConnectionPool -> Handler [Project]
listProjects pool = liftIO $ flip runSqlPersistMPool pool $ do
  maybeProjects <- selectList [] []
  let projects = map (\entity@(Entity _ project) -> project) maybeProjects
  return projects

listWorks :: ConnectionPool -> Handler [ElmWork]
listWorks pool = liftIO $ flip runSqlPersistMPool pool $ do
  entities <- selectList [WorkNote ==. ""] []
  let
    fromEntity (Entity workId work) = toElmWork workId work
    toElmWork workId (Work projectName from to note) = ElmWork
      (toElmWorkId workId)
      projectName
--      (utctDay from)
--      (utctDayTime from)
      eFrom eTo
      (maybeHour from to)
      (T.unpack note)
    eFrom = ElmDateTime (ElmDay 2020 1 1) (ElmTime 0 0)
    eTo = Just $ ElmDateTime (ElmDay 2020 1 1) (ElmTime 0 0)
    toElmWorkId key = Just $ ElmWorkId $ keyToInt key
    toElmProjectId key = ElmProjectId $ keyToInt key
    maybeDay to = case to of
      Just d -> Just $ utctDay d
      _ -> Nothing
    maybeDayTime to = case to of
      Just t -> Just $ utctDayTime t
      _ -> Nothing
    toElmTime (Just t) = (0, 0)
    toElmDay (Just t) = (0, 0, 0)
    keyToInt key = fromInt64 $ fromSqlKey key
  return $ map fromEntity entities

maybeHour from to = case to of
  Just t -> Just $ utcTimeDiff from t
  Nothing -> Nothing

getWorks :: ConnectionPool -> String -> Handler [ElmWork]
getWorks pool projectName = liftIO $ flip runSqlPersistMPool pool $ do
    works <- selectList [ WorkProjectName ==. projectName ] [ Asc WorkFrom ]
    return $ map (toElmWork projectName)  works


toElmWork projectName (Entity wid (Work _ from to notes)) = ElmWork
          (Just $ toElmWorkId wid)
          projectName
          (toElmDateTime from)
          (toElmDateTimeTo to)
          (maybeHour from to)
          (T.unpack notes)
  where
        toElmDateTime (UTCTime day time) =
          ElmDateTime (toElmDay day) (toElmTime time)
        toElmDateTimeTo to = case to of
          Nothing -> Nothing
          (Just (UTCTime day time)) ->
            Just $ ElmDateTime (toElmDay day) (toElmTime time)
        toElmDay day = case toGregorian day of
          (year, month, dom) -> ElmDay year month dom
        toElmTime diffTime = case timeToTimeOfDay diffTime of
            (TimeOfDay hour min _) -> ElmTime hour min

listItem :: DB -> Handler [ItemId]
listItem db = liftIO $ allItemIds db

getItem :: DB -> ItemId -> Handler Item
getItem db n = maybe (throwError err404) return =<< liftIO (lookupItem db n)

postItem :: DB -> String -> Handler ItemId
postItem db new = liftIO $ insertItem db new

postProject :: ConnectionPool -> Project -> Handler ()
postProject pool new = liftIO $ flip runSqlPersistMPool pool $ do
  _ <- insert new
  return ()

deleteWork :: ConnectionPool -> ElmWorkId -> Handler ()
deleteWork pool (ElmWorkId elmWorkId) = liftIO $ flip runSqlPersistMPool pool $
  delete wid
  where
    wid :: WorkId
    wid = toSqlKey $ toInt64 elmWorkId


fromInt64 :: Int64 -> Int
fromInt64 n = fromIntegral n
toInt64 :: Int -> Int64
toInt64 n = fromIntegral n

postWork :: ConnectionPool -> ElmWork -> Handler [ElmWork]
postWork pool elmWork = liftIO $ flip runSqlPersistMPool pool $ do
    let work = Work projectName from to notes'
        projectName = elmProjeclName elmWork
        notes' = T.pack $ notes elmWork
        from = UTCTime d t
          where
          d = case elmFrom elmWork of
            ElmDateTime d _ -> toDom d
          t = case elmFrom elmWork of
            ElmDateTime _ t ->
              timeOfDayToTime $ toTimeOfDay t
        to = case elmTo elmWork of
            Just (ElmDateTime elmDay elmTime) -> Just $ UTCTime d t
              where
                d = toDom elmDay
                t = timeOfDayToTime $ toTimeOfDay elmTime
            Nothing -> Nothing
        toDom (ElmDay year month dom) = fromGregorian year month dom
        toTimeOfDay (ElmTime hour min) = case makeTimeOfDayValid hour min 0 of
          Just timeOfDay -> timeOfDay
        hours = case to of
          Nothing -> Nothing
          Just t -> Just $ utcTimeDiff from t
    workId <- insert work
    works <- selectList [ WorkProjectName ==. projectName ] [ Asc WorkFrom ]
    return $ map (toElmWork projectName) works

utcTimeDiff from to =
  let sec = fromRational $ toRational $ diffUTCTime to from
  in sec / (60 * 60)

toElmWorkId :: WorkId -> ElmWorkId
toElmWorkId wid = ElmWorkId $ fromInt64 $ fromSqlKey wid
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
  modifyMVar_ mvar $ \m -> return (M.delete i m)
  return ()

