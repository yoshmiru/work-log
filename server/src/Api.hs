{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Proxy
import           Database.Persist
import           Servant.API
import qualified Elm.Derive
import           Elm.Module

import Models

type Api =
  "api" :>
    ("project" :> Get '[JSON] [Project] :<|>
     "project" :> ReqBody '[JSON] Project :> Post '[JSON] () :<|>
--     "work" :> ReqBody '[JSON] ElmWork :> Post '[JSON] ElmWorkId :<|>
     "item" :> Get '[JSON] [ItemId] :<|>
     "item" :> Capture "itemId" ItemId :> Get '[JSON] Item :<|>
     "item" :> ReqBody '[JSON] String :> Post '[JSON] ItemId :<|>
     "item" :> Capture "itemId" ItemId :> Delete '[JSON] ())

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

data ElmWork = ElmWork {
  workId :: ElmWorkId,
  elmProjectId :: ElmProjectId,
  from :: String,
  to :: String,
  notes :: String
} deriving (Show, Eq)

Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Project
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''ElmProjectId
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''ElmWork
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''ElmWorkId
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Work
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Item
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''ItemId
