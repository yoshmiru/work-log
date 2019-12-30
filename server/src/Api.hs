{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Proxy
import           Servant.API
import qualified Elm.Derive
import           Elm.Module

import Models

type Api =
  "api" :>
    ("projects" :> Get '[JSON] [Project] :<|>
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

Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Project
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Item
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''ItemId
