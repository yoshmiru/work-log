{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Proxy
import           Data.Time.Clock
import           Database.Persist
import           Servant.API
import qualified Elm.Derive
import           Elm.Module

import Models

type Api =
  "api" :>
    ("project" :> (
        Get '[JSON] [ElmProject] :<|>
        Capture "projectId" ElmProjectId :> Get '[JSON] (Maybe ElmProject) :<|>
        ReqBody '[JSON] Project :> Post '[JSON] () :<|>
        ReqBody '[JSON] ElmProjectId :> Delete '[JSON] ()) :<|>
     "work" :> (
        Get '[JSON] [ElmWork] :<|>
        Capture "projectId" ElmProjectId :> Get '[JSON] [ElmWork] :<|>
        Capture "elmWorkId" ElmWorkId :> Delete '[JSON] () :<|>
        ReqBody '[JSON] ElmWork :> Post '[JSON] [ElmWork]) :<|>
     "item" :> (Get '[JSON] [ItemId] :<|>
         Capture "itemId" ItemId :> Get '[JSON] Item :<|>
         ReqBody '[JSON] String :> Post '[JSON] ItemId :<|>
         Capture "itemId" ItemId :> Delete '[JSON] ()))

api :: Proxy Api
api = Proxy

-- types

newtype ItemId = ItemId Int
  deriving (Show, Eq, Ord, Enum, FromHttpApiData, ToHttpApiData)

data Item
  = Item {
    id :: ItemId,
    text :: String
  }
  deriving (Show, Eq)

newtype ElmProjectId = ElmProjectId Int
  deriving (Show, Eq, Ord, Enum, FromHttpApiData, ToHttpApiData)

newtype ElmWorkId = ElmWorkId Int
  deriving (Show, Eq, Ord, Enum, FromHttpApiData, ToHttpApiData)

data ElmDateTime = ElmDateTime {
  day :: ElmDay,
  time :: ElmTime
} deriving (Show, Eq)

data ElmDay = ElmDay {
  year :: Integer,
  month :: Int,
  dom :: Int
} deriving (Eq, Show)

data ElmTime = ElmTime {
  hour :: Int,
  min :: Int
} deriving (Show, Eq)

data ElmProject = ElmProject {
  projectId :: Maybe ElmProjectId,
  projectName :: String,
  projectUnitPrice :: Int
} deriving (Show, Eq)


data ElmWork = ElmWork {
  workId :: Maybe ElmWorkId,
  elmProjectId :: ElmProjectId,
  elmFrom :: ElmDateTime,
  elmTo :: Maybe ElmDateTime,
  hours :: Maybe Float,
  notes :: String
} deriving (Show, Eq)

Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Project
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''ElmProject
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''ElmProjectId
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''ElmDateTime
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''ElmDay
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''ElmTime
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''ElmWork
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''ElmWorkId
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Work
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Item
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''ItemId
