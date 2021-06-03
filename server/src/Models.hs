{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances #-}

module Models where

import Data.Aeson
import Data.Text
import Data.Time.Clock
import Data.Time.LocalTime

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- User
--   name Text
--   age  Int
--   UniqueName name
--   deriving Eq Read Show
Project
  name       Text
  unitPrice  Int
  archived   Bool default=False
  UniqueName name
  deriving Eq Read Show
Work
  projectId ProjectId
  from UTCTime
  to UTCTime Maybe
  note Text
  deriving Eq Read Show
|]

-- instance FromJSON User where
--   parseJSON = withObject "User" $ \ v ->
--     User <$> v .: "name"
--          <*> v .: "age"
-- 
-- instance ToJSON User where
--   toJSON (User name age) =
--     object [ "name" .= name
--            , "age"  .= age  ]

--instance FromJSON Project where
--  parseJSON = withObject "Project" $ \ v ->
--    Project <$> v .: "name"
--
--instance ToJSON Project where
--  toJSON (Project name) =
--    object [ "name" .= name ]
